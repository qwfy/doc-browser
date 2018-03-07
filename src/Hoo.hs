{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  ) where

import Data.Maybe
import Data.List

import qualified Hoogle

import qualified Entry
import Utils

search :: Int -> String -> IO [Entry.T]
search limit query = do
  -- TODO @incomplete: handle this properly
  let dbPath = "/home/incomplete/.config/doc-browser/hoogle/lts-10.8.hoo"
  Hoogle.withDatabase dbPath (\db ->
    Hoogle.searchDatabase db query
    |> take limit
    |> map toEntry
    |> return)

toEntry :: Hoogle.Target -> Entry.T
toEntry target =
  Entry.T { Entry.language = "Haskell"
          , Entry.version = "lts-10.8"
          , Entry.name = Hoogle.targetItem target
          -- TODO @incomplete: proper handling
          , Entry.path = fromMaybe "404.html" $ stripPrefix "file:///home/incomplete/.config/doc-browser/hoogle/lts-10.8/" (Hoogle.targetURL target)
          -- TODO @incomplete: replace this with ADT
          , Entry.source = "hoogle"
          -- TODO @incomplete: remove this field
          , Entry.nameLower = "dummy"
          }
