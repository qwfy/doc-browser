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
import qualified Data.ByteString.Char8 as Char8
import Data.Bits ((.|.))

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Text.Regex.PCRE

import Path
import qualified System.FilePath as FilePath

import Control.Applicative ((<|>))
import qualified Hoogle

import Database.Persist.Sqlite

import qualified Entry
import qualified Match
import qualified Hoo
import qualified Doc
import qualified Config
import qualified Slot
import qualified Db
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
  , ("go", [Doc.collection|Go|])
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

getQueryTextLower query =
  let queryStr = case query of
        Global s -> s
        Limited _ s -> s
  in map Data.Char.toLower queryStr

filterSearchables :: GeneralQuery -> [Entry.Searchable] -> [Entry.Searchable]
filterSearchables (Global _) es = es
filterSearchables (Limited abbr _) es =
  case Map.lookup abbr shortcuts of
    Nothing ->
      []
    Just collection ->
      filter ((== collection) . Entry.saCollection) es

search :: [Entry.Searchable] -> Int -> GeneralQuery -> [Entry.Searchable]
search allSearchables limit query =
  let queryStr = getQueryTextLower query
      searchables = filterSearchables query allSearchables
  in searchables
       |> map (distance queryStr . Entry.saNameLower)
       |> flip zip searchables
       |> filter (Data.Maybe.isJust . fst)
       |> Data.List.sortOn (\(dist, sa) -> (dist, Entry.saNameLower sa))
       |> take limit
       |> map snd

-- note: this is not a proper metric
distance :: String -> Char8.ByteString -> Maybe Float
distance query target =
  let a = subStringDistance (Char8.pack query) target
      b = regexDistance (queryToRegex query) (length query) target
  in a <|> b


-- TODO @incomplete: a match at the begining is better than a match at the end
subStringDistance :: Char8.ByteString -> Char8.ByteString -> Maybe Float
subStringDistance query target =
  if query `Char8.isInfixOf` target
    then
      let epsilon = 0.00001
          weight = fromIntegral (Char8.length target) / fromIntegral (Char8.length query)
      in Just $ epsilon * weight
    else
      Nothing


regexDistance :: Regex -> Int -> Char8.ByteString -> Maybe Float
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

          weight = fromIntegral (Char8.length target) / fromIntegral queryLength

          d = fromIntegral (Char8.length matchString - queryLength) / fromIntegral queryLength

      in Just $ d * weight


queryToRegex :: String -> Regex
queryToRegex query =
  query
    |> map escape
    |> Data.List.intercalate ".*?"
    |> Char8.pack
    |> makeRegexOpts compOpts defaultExecOpt
  where
    -- https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
    escape c
      | Data.Char.isAlphaNum c = [c]
      | otherwise = ['\\', c]
    compOpts = foldl (.|.) defaultCompOpt [compCaseless]


subString :: Char8.ByteString -> Int -> Int -> Char8.ByteString
subString str offset length' =
  str |> Char8.drop offset |> Char8.take length'

prefixHost :: Int -> String -> Text
prefixHost port path =
  let host = "http://localhost:" ++ show port
  in Text.pack $ host FilePath.</> path

startThread
  :: Config.T
  -> ConfigRoot
  -> Maybe (Path Abs File)
  -> Slot.T
  -> ([Match.T] -> IO ())
  -> IO ThreadId
startThread config configRoot hooMay slot handleMatches = do
  let dbPath = Db.dbPath configRoot |> toFilePath |> Text.pack
  forkIO $ runSqlite dbPath loop
  where
    loop :: Db.DbMonad a
    loop = do
      searchables <- Entry.loadSearchables
      forever $ do
        -- TODO @incomplete: make this limit configurable
        let limit = 27
        let prefixHost' = prefixHost $ Config.port config
        content <- liftIO . atomically $ takeTMVar (Slot.query slot)
        let (queryStr, matchHandler) = case content of
              Slot.GuiQuery x ->
                (x, handleMatches)
              Slot.HttpQuery x httpResultSlot ->
                (x, \ms -> liftIO . atomically $ updateTMVar httpResultSlot ms)

        (liftIO . matchHandler) =<<
          case Search.makeQuery queryStr of
              Nothing ->
                return []

              Just (G query) ->
                Search.search searchables limit query
                  |> Entry.toMatches prefixHost'

              Just (H (Hoogle query)) ->
                -- TODO @incomplete: run hoogle in a separate thread (to simplify some types for one reason)?
                case hooMay of
                  Nothing ->
                    -- TODO @incomplete: warn user about the lack of a hoogle database
                    return []
                  Just dbPath -> do
                    -- load the database on every search, instead of keeping it in memory,
                    -- this is done deliberately - turns out that it makes the GUI more responsive
                    collection <- liftIO . Doc.parseCollection . FilePath.takeBaseName . toFilePath $ dbPath
                    liftIO $ Hoogle.withDatabase (toFilePath dbPath) (\db ->
                               return $ Hoo.search configRoot collection db limit query prefixHost')
