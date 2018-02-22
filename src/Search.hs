{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Search
  ( search
  , makeQuery
  ) where

import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Char
import qualified Data.Array
import qualified Data.Tuple
import qualified Data.Maybe

import Control.Concurrent.STM.TVar

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


shortcuts = Map.fromList
  [ ("hs", "Haskell")
  , ("py", "Python")
  , ("tf", "TensorFlow")
  , ("np", "NumPy")
  , ("pd", "pandas")
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


-- TODO @incomplete: performance
search :: TVar [Entry.T] -> Query -> Int -> IO [Entry.T]
search entriesTVar query limit = do
  let queryStr = getQueryTextLower query
  entries <- filterEntry query <$> readTVarIO entriesTVar

  entries
    |> map (distance queryStr . Entry.name)
    |> (flip zip) entries
    |> filter (Data.Maybe.isJust . fst)
    |> Data.List.sort
    |> map snd
    |> take limit
    |> return


-- note: this is not a proper metric
distance :: String -> String -> Maybe Float
distance query target =
  let a = subStringDistance query target
      b = regexDistance (queryToRegex query) (length query) target
  in a <|> b


subStringDistance :: String -> String -> Maybe Float
subStringDistance query target =
  case query `Data.List.isInfixOf` (map Data.Char.toLower target) of
    False ->
      Nothing
    True ->
      let epsilon = 0.00001
          weight = fromIntegral (length target) / fromIntegral (length query)
      in Just $ epsilon * weight


regexDistance :: Regex -> Int -> String -> Maybe Float
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

          weight = fromIntegral (length target) / fromIntegral queryLength

          d = fromIntegral (length matchString - queryLength) / fromIntegral queryLength

      in Just $ d * weight


queryToRegex :: String -> Regex
queryToRegex query =
  query
    |> map escape
    |> Data.List.intercalate ".*?"
    |> makeRegexOpts compOpts defaultExecOpt
  where
    -- https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
    escape c
      | Data.Char.isAlphaNum c = [c]
      | otherwise = ['\\', c]
    compOpts = foldl (.|.) defaultCompOpt [compCaseless]


subString :: String -> Int -> Int -> String
subString str offset length' =
  str |> drop offset |> take length'
