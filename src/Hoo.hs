{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findDatabase
  ) where

import qualified Safe
import Data.Maybe
import Data.List
import System.Directory
import System.FilePath
import Control.Monad

import qualified Hoogle

import qualified Entry
import Utils

search :: Hoogle.Database -> Int -> String -> [Entry.T]
search db limit query =
  Hoogle.searchDatabase db query
  |> take limit
  |> map toEntry

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


findDatabase :: FilePath -> IO (Maybe FilePath)
findDatabase configRoot = do
  let hoogleDir = joinPath [configRoot, "hoogle"]
  exist <- doesDirectoryExist hoogleDir
  if not exist
    then return Nothing
    else do
      paths <- listDirectory hoogleDir >>= filterM (doesFileExist . (hoogleDir </>))
      filter isRecognized paths
        |> sort
        -- currently, only load the latest
        |> Safe.lastMay
        |> fmap ("hoogle" </>)
        |> return
  where
    isRecognized name =
      let a = takeExtension name == ".hoo"
          b = "lts-" `isPrefixOf` name
      in a && b
