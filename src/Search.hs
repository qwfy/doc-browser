{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Search
  ( search
  , makeQuery
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import qualified Text.Fuzzy as Fuzzy

import Control.Concurrent.STM.TVar

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

  Fuzzy.filter (Text.unpack txt) entries "" "" Entry.name False
    |> take limit
    |> map Fuzzy.original
    |> return
