{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module Upgrade
  ( start
  , Continue(..)
  ) where

import Control.Exception
import Control.Monad

import Data.Monoid
import Data.List

import System.IO
import Path
import Path.IO

import qualified Doc
import Utils


type DiskFormat = Int

-- This needs to be manually increased when incompatible changes are made to the disk format.
latestDiskFormat :: DiskFormat
latestDiskFormat = 2

-- What should the main program do when the upgrader's window is closed.
data Continue = Continue | Abort

data UpgradeError
  = HigherDiskFormat DiskFormat
  | UnRecognizedDiskFormat DiskFormat

instance Show UpgradeError where

  show (HigherDiskFormat userFormat) = intercalate "\n" $
    [ unwords
        [ "The latest version of the disk format supported by"
        , "this version of the program is"
        , show latestDiskFormat ++ ","
        , "but you have disk format version"
        , show userFormat ++ "."
        ]
    , "Possible fix: upgrade this program to a newer version."
    ]

  show (UnRecognizedDiskFormat userFormat) = intercalate "\n" $
    [ unwords
        [ "You have disk format version"
        , show userFormat ++ ","
        , "which this program does not recognize."
        ]
    , "Suggested action:"
    , "(1) Have you manually edited the disk format file and got it wrong?"
    , "(2) Update this program to a newer version."
    , "(3) Open an issue to report the problem."
    ]

instance Exception UpgradeError

type LineLogger = [String] -> IO ()


diskFormatFile configRoot =
  configRoot </> [relfile|disk-format|]

readDiskFormat :: ConfigRoot -> IO DiskFormat
readDiskFormat configRoot = do
  let formatFile = diskFormatFile configRoot
  exist <- doesFileExist formatFile
  if not exist
    then return 0
    else read <$> readFile (toFilePath formatFile)

writeDiskFormat :: ConfigRoot -> DiskFormat -> IO ()
writeDiskFormat configRoot diskFormat =
  writeFile (toFilePath $ diskFormatFile configRoot) (show diskFormat)

upgrade :: LineLogger -> ConfigRoot -> IO ()
upgrade logLine configRoot = do
  userFormat <- readDiskFormat configRoot

  if | userFormat == latestDiskFormat ->
       return ()

     | userFormat > latestDiskFormat ->
       throwIO $ HigherDiskFormat userFormat

     | otherwise -> do
       let succUserFormat = userFormat + 1
       logLine [ "Upgrading disk format from version"
               , show userFormat
               , "to"
               , show succUserFormat]

       case userFormat of
         0 ->
           upgradeFrom0 logLine configRoot
         1 ->
           upgradeFrom1 logLine configRoot
         _ ->
           throwIO $ UnRecognizedDiskFormat userFormat

       writeDiskFormat configRoot succUserFormat
       logLine [ "Done upgrading disk format from version"
               , show userFormat
               , "to"
               , show succUserFormat]
       upgrade logLine configRoot

upgradeFrom0 :: LineLogger -> ConfigRoot -> IO ()
upgradeFrom0 logLine configRoot = do
  let oldDevDocsDir = configRoot </> [reldir|devdocs|]
  newDevDocsDir <- (configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
  exist <- doesDirExist oldDevDocsDir
  when exist $ do
    logLine [ "Renaming directory"
            , toFilePath oldDevDocsDir
            , "to"
            , toFilePath newDevDocsDir]
    renameDir oldDevDocsDir newDevDocsDir

upgradeFrom1 :: LineLogger -> ConfigRoot -> IO ()
upgradeFrom1 logLine configRoot =
  forM_ [Doc.DevDocs, Doc.Hoogle] (\vendor -> do
    targetDir <- (configRoot </>) <$> (parseRelDir $ show vendor)
    logLine ["Ensure directory:", toFilePath targetDir]
    createDirIfMissing True targetDir)

data Log = Line String | Lines String

appendLog :: Handle -> Log -> IO ()
appendLog fh msg' = do
  time <- localTime
  let msg = case msg' of
        Line str ->
          unwords [time, str]
        Lines block ->
          let strs = block |> lines |> map ("--- " <>)
          in intercalate "\n" $ (time ++ " ---"):strs
  putStrLn msg
  hPutStrLn fh msg
  hFlush fh

start :: ConfigRoot -> IO Continue
start configRoot = do
  userFormat <- readDiskFormat configRoot
  if userFormat == latestDiskFormat
    then
      return Continue

    else
      withFile (toFilePath $ configRoot </> [relfile|disk-upgrade.log|]) AppendMode $ \logFileHandle -> do
        let appendLog' = appendLog logFileHandle

        let handleExceptions :: SomeException -> IO Continue
            handleExceptions e = do
              appendLog' . Lines . show $ e
              appendLog' . Line . unwords $
                [ "Upgrade has failed,"
                , "see the above error message for the reason."
                , "Please open an issue if you have trouble figuring out what's wrong."]
              return Abort

        appendLog' $ Line "Start to upgrade disk format."

        if userFormat < latestDiskFormat
          then do
            let upgrade' = do
                  upgrade (appendLog' . Line. unwords) configRoot
                  appendLog' $ Line "Upgrade finished successfully."
                  return Continue
            upgrade' `catch` handleExceptions

          else
            throwIO (HigherDiskFormat userFormat) `catch` handleExceptions
