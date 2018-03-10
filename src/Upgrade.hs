{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

module Upgrade
  ( startGUI
  , Continue(..)
  ) where

import Control.Exception
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad

import Graphics.QML

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

import System.FilePath
import System.Directory

import qualified Doc
import Paths_doc_browser


type DiskFormat = Int

-- This needs to be manually increased when incompatible changes are made to the disk format.
latestDiskFormat :: DiskFormat
latestDiskFormat = 1

-- What should the main program do when the upgrader's window is closed.
data Continue = Continue | Abort

data UpgradeError
  = HigherDiskFormat DiskFormat
  | UnRecognizedDiskFormat DiskFormat

instance Show UpgradeError where

  show (HigherDiskFormat userFormat) = unwords
    [ "The latest version of the disk format supported by"
    , "this version of the program is"
    , show latestDiskFormat ++ ","
    , "but you have disk format version"
    , show userFormat ++ "."
    , "Possible fix: upgrade this program to a newer version."]

  show (UnRecognizedDiskFormat userFormat) = unwords
    [ "You have disk format version"
    , show userFormat ++ ","
    , "which this program does not recognize."
    , "Suggested action:"
    , "(1) Have you manually edited the disk format file and got it wrong?"
    , "(2) Update this program to a newer version."
    , "(3) Open an issue to report the problem."]

instance Exception UpgradeError

type Reporter = [String] -> IO ()


diskFormatFile configRoot =
  configRoot </> "disk-format"

readDiskFormat :: FilePath -> IO DiskFormat
readDiskFormat configRoot = do
  let formatFile = diskFormatFile configRoot
  exist <- doesFileExist formatFile
  if not exist
    then return 0
    else read <$> readFile formatFile

writeDiskFormat :: FilePath -> DiskFormat -> IO ()
writeDiskFormat configRoot diskFormat =
  writeFile (diskFormatFile configRoot) (show diskFormat)

upgrade :: Reporter -> FilePath -> IO ()
upgrade report configRoot = do
  userFormat <- readDiskFormat configRoot

  if | userFormat == latestDiskFormat ->
       return ()

     | userFormat > latestDiskFormat ->
       throwIO $ HigherDiskFormat userFormat

     | otherwise -> do
       let succUserFormat = userFormat + 1
       report [ "Upgrading disk format from version"
              , show userFormat
              , "to"
              , show succUserFormat]

       case userFormat of
         0 ->
           upgradeFrom0 report configRoot
         _ ->
           throwIO $ UnRecognizedDiskFormat userFormat

       writeDiskFormat configRoot succUserFormat
       report [ "Done upgrading disk format from version"
              , show userFormat
              , "to"
              , show succUserFormat]
       upgrade report configRoot

upgradeFrom0 :: Reporter -> FilePath -> IO ()
upgradeFrom0 report configRoot = do
  let oldDevDocsDir = configRoot </> "devdocs"
  let newDevDocsDir = configRoot </> show Doc.DevDocs
  exist <- doesDirectoryExist oldDevDocsDir
  when exist $ do
    report [ "Renaming directory"
           , oldDevDocsDir
           , "to"
           , newDevDocsDir]
    renameDirectory oldDevDocsDir newDevDocsDir

startGUI :: FilePath -> IO Continue
startGUI configRoot = do

  msgVar <- atomically $ newTVar ("" :: Text)
  msgKey <- newSignalKey :: IO (SignalKey (IO ()))

  classContext <- newClass
    [ defPropertySigRO' "upgradeMessage" msgKey
        (\_obj -> readTVarIO msgVar)
    ]

  objectContext <- newObject classContext ()

  let appendMsg :: String -> IO ()
      appendMsg msg = do
        let appendTo old = old <> lineBreak <> "==> " <> Text.pack msg
              where lineBreak = if old == "" then "" else "<br>"
        atomically $ modifyTVar msgVar appendTo

  let handleExceptions :: SomeException -> IO Continue
      handleExceptions e = do
        appendMsg . show $ e
        appendMsg . unwords $
          [ "Upgrade has failed,"
          , "see the above error message for the reason."
          , "Please open an issue if you have trouble figuring out what's wrong."]
        return Abort

  userFormat <- readDiskFormat configRoot
  if userFormat == latestDiskFormat
    then
      return Continue

    else do
      mainQml <- getDataFileName "ui/disk-upgrade.qml"
      let engineConfig = defaultEngineConfig
            { initialDocument = fileDocument mainQml
            , contextObject = Just $ anyObjRef objectContext
            }

      appendMsg "Start to upgrade disk format"
      if userFormat < latestDiskFormat
        then do
          let upgrade' = do
                upgrade (appendMsg . unwords) configRoot
                appendMsg "Upgrade finished successfully. Please close this window to continue."
                return Continue
          upgradeResult <- async (upgrade' `catch` handleExceptions)
          runEngineLoop engineConfig
          wait upgradeResult

        else do
          upgradeResult <- throwIO (HigherDiskFormat userFormat) `catch` handleExceptions
          runEngineLoop engineConfig
          return upgradeResult
