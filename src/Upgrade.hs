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
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.List

import System.IO
import qualified System.FilePath as FilePath

import Path
import Path.IO

import Graphics.QML

import Fmt

import qualified Doc
import qualified Hoo
import qualified Db
import qualified DevDocs
import Utils


-- TODO @incomplete: replace this with a sum type, so that exhaustive check works
type DiskFormat = Int

-- This needs to be manually increased when incompatible changes are made to the disk format.
latestDiskFormat :: DiskFormat
latestDiskFormat = 5

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
type LinesLogger = String -> IO ()


diskFormatFile configRoot =
  getConfigRoot configRoot </> [relfile|disk-format|]

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

upgrade :: LineLogger -> LinesLogger -> ConfigRoot -> IO ()
upgrade logLine logLines configRoot = do
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
         2 ->
           upgradeFrom2 logLine configRoot
         3 ->
           upgradeFrom3 logLine logLines configRoot
         4 ->
           upgradeFrom4 logLine configRoot
         _ ->
           throwIO $ UnRecognizedDiskFormat userFormat

       writeDiskFormat configRoot succUserFormat
       logLine [ "Done upgrading disk format from version"
               , show userFormat
               , "to"
               , show succUserFormat]
       upgrade logLine logLines configRoot

upgradeFrom0 :: LineLogger -> ConfigRoot -> IO ()
upgradeFrom0 logLine configRoot = do
  let oldDevDocsDir = getConfigRoot configRoot </> [reldir|devdocs|]
  newDevDocsDir <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
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
    targetDir <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show vendor)
    logLine ["Ensure directory:", toFilePath targetDir]
    createDirIfMissing True targetDir)

upgradeFrom2 :: LineLogger -> ConfigRoot -> IO ()
upgradeFrom2 logLine configRoot = do
  databaseFiles <- Hoo.findDatabases configRoot
  forM_ databaseFiles $ \databaseFile -> do
    docDir <- parseAbsDir $ (FilePath.dropExtension $ fromAbsFile databaseFile)
    exist <- doesDirExist docDir
    when exist $ do
      logLine ["Removing old Hoogle database:", toFilePath databaseFile]
      tryRemoveFile databaseFile
      databaseFile -<.> "warn" >>= tryRemoveFile
      logLine ["Creating new Hoogle database for documentations located at:", toFilePath docDir ++ ",", "this may take a while"]
      Hoo.installFromDir docDir
      logLine ["Hoogle database generated"]

upgradeFrom3 :: LineLogger -> LinesLogger -> ConfigRoot -> IO ()
upgradeFrom3 logLine logLines configRoot = do
  logLine ["Scanning old docsets"]
  cvs <- scan configRoot
  let dbPath = Db.dbPath configRoot
  logLine ["Creating database at:", toFilePath dbPath]
  Db.migrateAll dbPath >>= logLines . unlines
  logLine ["Database created"]
  unless (null cvs) $ do
    logLine ["Loading", show $ length cvs, "old docsets"]
    forM_ (zip [1..] cvs) (installOne $ length cvs)
  where
    -- scan devdocs' root, return collections and versions
    scan :: ConfigRoot -> IO [(Doc.Collection, Doc.Version)]
    scan configRoot = do
      dir <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
      (dirs, _) <- listDir dir
      mapM Doc.breakCollectionVersion dirs

    installOne :: Int -> (Int, (Doc.Collection, Doc.Version)) -> IO ()
    installOne total (index, (collection, version)) = do
      let padLeft = show total |> length |> (\len -> padLeftF len '0')
      let docId = Doc.combineCollectionVersion collection version
      logLine [""+|padLeft index|+"/"+|total|+":" :: String, docId]
      vendorHome <- parseRelDir $ show Doc.DevDocs
      collectionHome <- parseRelDir $ docId
      let indexJson = getConfigRoot configRoot </> vendorHome </> collectionHome </> [relfile|index.json|]
      DevDocs.insertToDb configRoot collection version indexJson

upgradeFrom4 :: LineLogger -> ConfigRoot -> IO ()
upgradeFrom4 logLine configRoot = do
  targetDir <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.Dash)
  logLine ["Ensure directory:", toFilePath targetDir]
  createDirIfMissing True targetDir

data Log = Line String | Lines String

appendLog :: Handle -> (String -> IO ()) -> Log -> IO ()
appendLog fh writeGui msg' = do
  time <- localTime
  let msg = case msg' of
        Line str ->
          unwords [time, str]
        Lines block ->
          let strs = block |> lines |> map ("--- " <>)
          in intercalate "\n" $ (time ++ " ---"):strs
  putStrLn msg
  hPutStrLn fh msg
  writeGui msg
  hFlush fh

start :: ConfigRoot -> Path a Dir -> IO Continue
start configRoot guiDir = do
  userFormat <- readDiskFormat configRoot
  if userFormat == latestDiskFormat
    then
      return Continue

    else
      withFile (toFilePath $ getConfigRoot configRoot </> [relfile|disk-upgrade.log|]) AppendMode $ \logFileHandle -> do

        (withGui, guiWriter) <- setupGui guiDir

        let appendLog' = appendLog logFileHandle guiWriter

        let handleExceptions :: SomeException -> IO Continue
            handleExceptions e = do
              appendLog' . Lines . show $ e
              appendLog' . Line . unwords $
                [ "Upgrade has failed,"
                , "see the above error message for the reason."
                , "Please open an issue if you have trouble figuring out what's wrong."]
              return Abort

        let doit = do
              appendLog' $ Line "Start to upgrade disk format."
              if userFormat < latestDiskFormat
                then do
                  let upgrade' = do
                        upgrade (appendLog' . Line. unwords) (appendLog' . Lines) configRoot
                        appendLog' $ Line "Upgrade finished successfully."
                        return Continue
                  upgrade' `catch` handleExceptions

                else
                  throwIO (HigherDiskFormat userFormat) `catch` handleExceptions

        withGui doit

setupGui :: Path a Dir -> IO (IO Continue -> IO Continue, String -> IO ())
setupGui guiDir = do
  msgsVar <- atomically $ newTVar ("" :: Text)
  msgsKey <- newSignalKey :: IO (SignalKey (IO ()))

  classContext <- newClass
    [ defPropertySigRO' "messages" msgsKey
        (\_obj -> readTVarIO msgsVar)
    ]

  objectContext <- newObject classContext ()

  let write :: String -> IO ()
      write str' = do
        let str = Text.pack str'
        atomically $ modifyTVar msgsVar (\old -> if Text.null old then str else old <> "\n" <> str)
        fireSignal msgsKey objectContext

  let mainQml = guiDir </> [relfile|ui/disk-upgrade.qml|]

  let engineConfig = defaultEngineConfig
        { initialDocument = fileDocument $ toFilePath mainQml
        , contextObject = Just $ anyObjRef objectContext
        }

  let withGui action = do
        done <- async action
        runEngineLoop engineConfig
        wait done

  return (withGui, write)
