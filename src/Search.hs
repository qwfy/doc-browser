{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Search
  ( search
  , makeQuery
  ) where

import Data.Text (Text)
import qualified Data.List
import qualified Data.Text as Text
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
  = Global Text
  | Limited String Text
  deriving (Show)

makeQuery :: Text -> Maybe Query
makeQuery txt =
  let str = Text.unpack txt
  in case str of
       [] ->
         Nothing
       ('/':c1:c2:c3:t) ->
         -- limit using prefix, like this: /pysigmoid
         Just $ Limited [c1, c2] (Text.pack (c3:t))
       ('/':_) ->
         -- cannot start a search with /
         Nothing
       _ ->
         case reverse str of
           (c1:c2:'/':c3:t) ->
             -- limit using suffix, like this: sigmoid/py
             Just $ Limited [c2, c1] (Text.pack . reverse $ (c3:t))
           _ ->
             Just . Global $ txt

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

getQueryText (Global t) = t
getQueryText (Limited _ t) = t

-- TODO @incomplete: case insensitive
-- TODO @incomplete: performance
search :: TVar [Entry.T] -> Query -> Int -> IO [Entry.T]
search entriesTVar query limit = do
  let txt = getQueryText query
  entries <- filterEntry query <$> readTVarIO entriesTVar

  entries
    |> map (distance (compileQuery txt) txt . Entry.name)
    |> (flip zip) entries
    |> filter (Data.Maybe.isJust . fst)
    |> Data.List.sort
    |> map snd
    |> take limit
    |> return

compileQuery :: Text -> Regex
compileQuery query =
  query
    |> Text.unpack
    |> map escape
    |> Data.List.intercalate ".*?"
    |> makeRegexOpts compOpts defaultExecOpt
  where
    -- https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
    escape c
      | Data.Char.isAlphaNum c = [c]
      | otherwise = ['\\', c]
    compOpts = foldl (.|.) defaultCompOpt [compCaseless]

-- note: this is not a proper metric
distance :: Regex -> Text -> String -> Maybe Float
distance regex query target =
  subStringDistance query (Text.pack target) <|> regexDistance regex (Text.unpack query) target

subStringDistance :: Text -> Text -> Maybe Float
subStringDistance query target =
  case Text.toLower query `Text.isInfixOf` Text.toLower target of
    False ->
      Nothing
    True ->
      let epsilon = 0.00001
          weight = fromIntegral (Text.length target) / fromIntegral (Text.length query)
      in Just $ epsilon * weight

regexDistance :: Regex -> String -> String -> Maybe Float
regexDistance query queryStr target =
  case matchAll query target of
    [] ->
      Nothing
    matchesArray ->
      let (matchOffset, matchLength) = matchesArray
            |> map (Data.Array.! 0)
            |> Data.List.sortBy (\a b -> compare (Data.Tuple.swap a) (Data.Tuple.swap b))
            |> head

          matchString = subString target matchOffset matchLength

          weight = fromIntegral (length target) / fromIntegral (length queryStr)

          d = fromIntegral (length matchString - length queryStr) / fromIntegral (length queryStr)

      in Just $ d * weight

subString :: String -> Int -> Int -> String
subString str offset length' =
  str |> drop offset |> take length'
