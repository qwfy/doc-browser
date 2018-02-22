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

import Control.Concurrent.STM.TVar

import Text.EditDistance

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

  let measure = Text.EditDistance.levenshteinDistance
        editCosts (Text.unpack $ Text.toLower txt)
  entries
    |> map (measure . Entry.name)
    |> (flip zip) entries
    |> filter ((< 50) . fst)
    |> Data.List.sort
    |> map snd
    |> take limit
    |> return
  where
    editCosts = Text.EditDistance.EditCosts
      { deletionCosts = Text.EditDistance.ConstantCost 10
      , insertionCosts = Text.EditDistance.ConstantCost 1
      , substitutionCosts = Text.EditDistance.ConstantCost 10

      -- this field is not used, see here:
      -- https://hackage.haskell.org/package/edit-distance-0.2.2.1/docs/Text-EditDistance.html#v:levenshteinDistance
      , transpositionCosts = Text.EditDistance.ConstantCost 0
      }
