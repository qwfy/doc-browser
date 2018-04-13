{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Search
  ( search
  , makeQuery
  , startThread
  , queryToGoogle
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Char
import qualified Data.Array
import qualified Data.Tuple
import qualified Data.Maybe
import qualified Data.ByteString.Char8 as C
import Data.Bits ((.|.))

import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Text.Regex.PCRE

import Path
import qualified System.FilePath as FilePath

import Control.Applicative ((<|>))
import qualified Hoogle

import qualified Entry
import qualified Match
import qualified Hoo
import qualified Doc
import qualified Config
import qualified Slot
import Utils

data Query
  = G GeneralQuery
  | H HoogleQuery
  deriving (Show)

data GeneralQuery
  = Global String
  | Limited String String
  deriving (Show)

newtype HoogleQuery
  = Hoogle String
  deriving (Show)

makeQuery :: String -> Maybe Query
makeQuery str =
  case str of
    [] ->
      Nothing

    ('/':'h':'h':c:t) ->
      Just . H . Hoogle $ (c:t)

    ('/':c1:c2:c3:t) ->
      -- limit using prefix, like this: /tfsigmoid
      Just . G $ Limited [c1, c2] (c3:t)

    ('/':_) ->
      -- cannot start a search with /
      Nothing

    _ ->
      case reverse str of
        ('h':'h':'/':c:t) ->
          Just . H . Hoogle . reverse $ (c:t)
        (c1:c2:'/':c3:t) ->
          -- limit using suffix, like this: sigmoid/tf
          Just . G $ Limited [c2, c1] (reverse (c3:t))
        _ ->
          Just . G . Global $ str


shortcuts = Map.fromList
  [ ("hs", [Doc.collection|Haskell|])
  , ("py", [Doc.collection|Python|])
  , ("tf", [Doc.collection|TensorFlow|])
  , ("np", [Doc.collection|NumPy|])
  , ("pd", [Doc.collection|pandas|])
  , ("er", [Doc.collection|Erlang|])
  , ("mp", [Doc.collection|Matplotlib|])
  ]

queryToGoogle :: Query -> String
queryToGoogle (H (Hoogle str)) = unwords ["Haskell", str]
queryToGoogle (G (Global str)) = str
queryToGoogle (G (Limited abbr str)) =
  case Map.lookup abbr shortcuts of
    Nothing ->
      str
    Just collection ->
      unwords [show collection, str]

filterEntry :: GeneralQuery -> [Entry.T] -> [Entry.T]
filterEntry (Global _) es = es
filterEntry (Limited abbr _) es =
  case Map.lookup abbr shortcuts of
    Nothing ->
      []
    Just collection ->
      filter ((== collection) . Entry.collection) es


getQueryTextLower query =
  let queryStr = case query of
        Global s -> s
        Limited _ s -> s
  in map Data.Char.toLower queryStr


search :: [Entry.T] -> Int -> GeneralQuery -> [Entry.T]
search allEntries limit query =
  let queryStr = getQueryTextLower query
      entries = filterEntry query allEntries
  in entries
       |> map (distance queryStr . Entry.nameLower)
       |> flip zip entries
       |> filter (Data.Maybe.isJust . fst)
       |> Data.List.sort
       |> map snd
       |> take limit


-- note: this is not a proper metric
distance :: String -> C.ByteString -> Maybe Float
distance query target =
  let a = subStringDistance (C.pack query) target
      b = regexDistance (queryToRegex query) (length query) target
  in a <|> b


-- TODO @incomplete: a match at the begining is better than a match at the end
subStringDistance :: C.ByteString -> C.ByteString -> Maybe Float
subStringDistance query target =
  if query `C.isInfixOf` target
    then
      let epsilon = 0.00001
          weight = fromIntegral (C.length target) / fromIntegral (C.length query)
      in Just $ epsilon * weight
    else
      Nothing


regexDistance :: Regex -> Int -> C.ByteString -> Maybe Float
regexDistance regex queryLength target =
  case matchAll regex target of
    [] ->
      Nothing
    matchesArray ->
      let (matchOffset, matchLength) = matchesArray
            |> map (Data.Array.! 0)
            |> Data.List.sortBy (\a b -> compare (Data.Tuple.swap a) (Data.Tuple.swap b))
            |> head

          matchString = subString target matchOffset matchLength

          weight = fromIntegral (C.length target) / fromIntegral queryLength

          d = fromIntegral (C.length matchString - queryLength) / fromIntegral queryLength

      in Just $ d * weight


queryToRegex :: String -> Regex
queryToRegex query =
  query
    |> map escape
    |> Data.List.intercalate ".*?"
    |> C.pack
    |> makeRegexOpts compOpts defaultExecOpt
  where
    -- https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
    escape c
      | Data.Char.isAlphaNum c = [c]
      | otherwise = ['\\', c]
    compOpts = foldl (.|.) defaultCompOpt [compCaseless]


subString :: C.ByteString -> Int -> Int -> C.ByteString
subString str offset length' =
  str |> C.drop offset |> C.take length'

prefixHost :: Int -> String -> Text
prefixHost port path =
  let host = "http://localhost:" ++ show port
  in Text.pack $ host FilePath.</> path

startThread
  :: Config.T
  -> ConfigRoot
  -> [Entry.T]
  -> Maybe (Path Abs File)
  -> Slot.T
  -> ([Match.T] -> IO ())
  -> IO ThreadId
startThread config configRoot entries hooMay slot handleMatches =
  forkIO loop
  where
    loop = forever $ do
      -- TODO @incomplete: make this limit configurable
      let limit = 27
      let prefixHost' = prefixHost $ Config.port config
      content <- atomically $ takeTMVar (Slot.query slot)
      let (queryStr, matchHandler) = case content of
            Slot.GuiQuery x ->
              (x, handleMatches)
            Slot.HttpQuery x httpResultSlot ->
              (x, \ms -> atomically $ updateTMVar httpResultSlot ms)

      matchHandler =<<
        case Search.makeQuery queryStr of
            Nothing ->
              return []

            Just (G query) ->
              Search.search entries limit query
                |> map (Entry.toMatch prefixHost')
                |> return

            Just (H (Hoogle query)) ->
              case hooMay of
                Nothing ->
                  -- TODO @incomplete: warn user about the lack of a hoogle database
                  return []
                Just dbPath -> do
                  -- load the database on every search, instead of keeping it in memory,
                  -- this is done deliberately - turns out that it makes the GUI more responsive
                  collection <- Doc.parseCollection . FilePath.takeBaseName . toFilePath $ dbPath
                  Hoogle.withDatabase (toFilePath dbPath) (\db -> return $ Hoo.search configRoot collection db limit query prefixHost')
