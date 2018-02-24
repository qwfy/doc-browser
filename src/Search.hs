{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Search
  ( search
  , makeQuery
  , startThread
  ) where

import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Char
import qualified Data.Array
import qualified Data.Tuple
import qualified Data.Maybe
import qualified Data.ByteString.Char8 as C

import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Text.Regex.PCRE
import Data.Bits ((.|.))

import Control.Applicative ((<|>))

import qualified Entry
import Utils


data Query
  = Global String
  | Limited String String
  deriving (Show)


makeQuery :: String -> Maybe Query
makeQuery str =
  case str of
    [] ->
      Nothing

    ('/':c1:c2:c3:t) ->
      -- limit using prefix, like this: /tfsigmoid
      Just $ Limited [c1, c2] (c3:t)

    ('/':_) ->
      -- cannot start a search with /
      Nothing

    _ ->
      case reverse str of
        (c1:c2:'/':c3:t) ->
          -- limit using suffix, like this: sigmoid/tf
          Just $ Limited [c2, c1] (reverse $ (c3:t))
        _ ->
          Just . Global $ str


-- shortcuts :: [(AbbrStriing, Language)]
shortcuts = Map.fromList
  [ ("hs", "Haskell")
  , ("py", "Python")
  , ("tf", "TensorFlow")
  , ("np", "NumPy")
  , ("pd", "pandas")
  , ("er", "Erlang")
  ]


filterEntry :: Query -> [Entry.T] -> [Entry.T]
filterEntry (Global _) es = es
filterEntry (Limited abbr _) es =
  case Map.lookup abbr shortcuts of
    Nothing ->
      []
    Just language ->
      filter (\entry -> Entry.language entry == language) es


getQueryTextLower query =
  let queryStr = case query of
        Global s -> s
        Limited _ s -> s
  in map Data.Char.toLower queryStr


search :: [Entry.T] -> Int -> Query -> [Entry.T]
search allEntries limit query =
  let queryStr = getQueryTextLower query
      entries = filterEntry query allEntries
  in entries
       |> map (distance queryStr . Entry.nameLower)
       |> (flip zip) entries
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


subStringDistance :: C.ByteString -> C.ByteString -> Maybe Float
subStringDistance query target =
  case query `C.isInfixOf` target of
    False ->
      Nothing
    True ->
      let epsilon = 0.00001
          weight = fromIntegral (C.length target) / fromIntegral (C.length query)
      in Just $ epsilon * weight


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

startThread :: [Entry.T] -> TMVar String -> ([Entry.T] -> IO ())-> IO ThreadId
startThread entries querySlot handleEntries =
  forkIO . forever $ do
    queryStr <- atomically $ takeTMVar querySlot
    let matches = case Search.makeQuery queryStr of
          Nothing -> []
          -- TODO @incomplete: make this limit configurable
          Just query -> Search.search entries 27 query
    handleEntries matches
