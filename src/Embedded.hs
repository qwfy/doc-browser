{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Embedded
  ( configYaml
  , extractUIDirInto
  ) where

import Data.FileEmbed
import qualified Data.ByteString as BS

import qualified System.FilePath as FilePath

import Control.Monad

import Path
import Path.IO

import Utils

configYaml :: BS.ByteString
configYaml = $(embedFile "config.yaml")

extractUIDirInto :: Path a Dir -> IO ()
extractUIDirInto parentDir = do
  let uiFiles :: [(FilePath.FilePath, BS.ByteString)]
      uiFiles = $(embedDir "ui")
  forM_ uiFiles $ \(filePath, content) -> do
    targetPath <- ((parentDir </> [reldir|ui|]) </>) <$> (parseRelFile filePath)
    hasJsc <- hasExtension targetPath "jsc"
    hasQmlc <- hasExtension targetPath "qmlc"
    let shouleIgnore = or
          [ filePath == "doc-browser.pro"
          , filePath == "doc-browser.pro.user"
          , hasJsc
          , hasQmlc
          ]
    unless shouleIgnore $ do
      createDirIfMissing True (parent targetPath)
      BS.writeFile (toFilePath targetPath) content
