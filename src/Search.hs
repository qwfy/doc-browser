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
  = Global BareQs
  | LimitToDevDocs Doc.Collection Config.LowerCasePrefix BareQs
  | LimitToDash    Doc.Collection Config.LowerCasePrefix BareQs
  deriving (Show)

newtype HoogleQuery
  -- Search the latest hoogle database
  -- TODO @incomplete: define latest
  = HoogleLatest BareQs
  deriving (Show)

-- Search string without command
newtype BareQs = BareQs {getBareQs :: String}
  deriving (Show)

makeQuery :: Config.Commands -> String -> Maybe Query
makeQuery commands str =
  case relocateCommand str of
    ('/':c1:c2:qs) ->
      if null qs
        then Nothing
        else
          case Map.lookup (Config.makeAbbr c1 c2) commands of
            Nothing                               -> Nothing
            Just Config.HoogleLatest              -> Just . H $ HoogleLatest (BareQs qs)
            Just (Config.LimitToDevDocs coll lcp) -> Just . G $ LimitToDevDocs coll lcp (BareQs qs)
            Just (Config.LimitToDash coll lcp)    -> Just . G $ LimitToDash    coll lcp (BareQs qs)

    [] ->
      Nothing

    xs ->
      Just . G . Global $ BareQs xs

  where
    relocateCommand xs =
      case xs of
        ('/':_:_:_) ->
          -- if it starts with a command, then don't consider the tail,
          -- i.e. we prefer prefixed command
          xs
        _ ->
          case reverse xs of
            (c1:c2:'/':t) -> '/':c2:c1:reverse t
            _ -> xs


queryToGoogle :: Query -> String
queryToGoogle (H (HoogleLatest qs)) = unwords ["Haskell", getBareQs qs]
queryToGoogle (G (Global qs)) = getBareQs qs
queryToGoogle (G (LimitToDevDocs collection _ qs)) = unwords [show collection, getBareQs qs]
queryToGoogle (G (LimitToDash collection _ qs)) = unwords [show collection, getBareQs qs]

getBareQsFromGeneralQuery query =
  case query of
    Global qs             -> qs
    LimitToDevDocs _ _ qs -> qs
    LimitToDash _ _ qs    -> qs

filterSearchables :: GeneralQuery -> [Entry.Searchable] -> [Entry.Searchable]
filterSearchables (Global _) es = es
filterSearchables (LimitToDevDocs collection versionLcp _) es =
  -- TODO @incomplete: handle versionLcp
  -- TODO @incomplete: handle vendor
  filter (\e -> Entry.saCollection e == collection) es
filterSearchables (LimitToDash collection versionLcp _) es =
  filter (\e -> Entry.saCollection e == collection) es

search :: [Entry.Searchable] -> Int -> GeneralQuery -> [Entry.Searchable]
search allSearchables limit query =
  let queryStr = getBareQsFromGeneralQuery query |> getBareQs |> map Data.Char.toLower
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
  forkIO $ runSqlite (Db.dbPathText configRoot) loop
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
          case Search.makeQuery (Config.commands config) queryStr of
              Nothing ->
                return []

              Just (G query) ->
                Search.search searchables limit query
                  |> Entry.toMatches prefixHost'

              Just (H (HoogleLatest query)) ->
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
                               return $ Hoo.search configRoot collection db limit (getBareQs query) prefixHost')
