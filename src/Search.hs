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

import System.Clock

import Text.EditDistance

import qualified Entry

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

-- -- TODO @incomplete: case insensitive
-- -- TODO @incomplete: performance
-- search :: TVar [Entry.T] -> Query -> Int -> IO [Entry.T]
-- search entriesTVar query limit = do
--   let txt = getQueryText query
--   entries <- filterEntry query <$> readTVarIO entriesTVar
--
--   let measure = Text.EditDistance.levenshteinDistance
--         editCosts (Text.unpack $ Text.toLower txt)
--   let distances = map (measure . Entry.name) entries
--   let tagged = zip distances entries
--   let sorted = map snd $ Data.List.sort tagged
--   return $ take limit sorted
--   where
--     editCosts = Text.EditDistance.EditCosts
--       { deletionCosts = Text.EditDistance.ConstantCost 10
--       , insertionCosts = Text.EditDistance.ConstantCost 1
--       , substitutionCosts = Text.EditDistance.ConstantCost 10
--       , transpositionCosts = Text.EditDistance.ConstantCost 5
--       }


-- modified version of search, used to do benchmarking
search' allEntries query limit = do
  let txt = getQueryText query
  clock1 <- getTime Monotonic

  let clock2 = clock1

  let entries = filterEntry query allEntries
  clock3 <- getTime Monotonic

  let measure = Text.EditDistance.levenshteinDistance
        editCosts (Text.unpack $ Text.toLower txt)
  let distances = map (measure . Entry.name) entries
  clock4 <- getTime Monotonic

  let tagged = zip distances entries
  clock5 <- getTime Monotonic

  let sorted = map snd $ Data.List.sort tagged
  clock6 <- getTime Monotonic

  return $ (take limit sorted, Text.length txt, [clock1, clock2, clock3, clock4, clock5, clock6])
  where
    editCosts = Text.EditDistance.EditCosts
      { deletionCosts = Text.EditDistance.ConstantCost 10
      , insertionCosts = Text.EditDistance.ConstantCost 1
      , substitutionCosts = Text.EditDistance.ConstantCost 10
      , transpositionCosts = Text.EditDistance.ConstantCost 5
      }

showClock :: TimeSpec -> TimeSpec -> String
showClock (TimeSpec {sec=minSec, nsec=minNsec}) (TimeSpec {sec, nsec}) =
  let zero = (fromIntegral minSec) * 1000000000 + (fromIntegral minNsec)
      x = (fromIntegral sec) * 1000000000 + (fromIntegral nsec)
  in show $ x - zero

printStat len clockBegin clocks =
  let cols = show len : map (showClock clockBegin) clocks
      strs = Data.List.intercalate ", " cols
  in putStrLn strs

search a b c = do
  clockBegin <- getTime Monotonic
  (x, len, clocks) <- search' a b c
  clockEnd <- getTime Monotonic
  printStat len clockBegin (clocks ++ [clockEnd])
  return x
