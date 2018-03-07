{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findDatabase
  ) where

import qualified Safe
import Data.List
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Control.Monad

import qualified Hoogle

import qualified Match
import Utils

search :: Hoogle.Database -> Int -> String -> [Match.T]
search db limit query =
  Hoogle.searchDatabase db query
  |> take limit
  |> map toMatch

toMatch :: Hoogle.Target -> Match.T
toMatch target =
  Match.T { Match.language = "Haskell"
          , Match.version = "lts-10.8"
          , Match.name = Text.pack $ Hoogle.targetItem target
          -- TODO @incomplete: proper handling
          , Match.url = Text.pack $ Hoogle.targetURL target
          , Match.source = "hoogle"
          , Match.package_ = Text.pack . fst <$> Hoogle.targetPackage target
          , Match.module_ = Text.pack . fst <$> Hoogle.targetModule target
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
