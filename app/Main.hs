{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Graphics.QML

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.Extra
import qualified Data.ByteString.Char8 as Char8
import Data.Aeson
import qualified Data.Yaml as Yaml

import System.Hclip
import System.Environment
import Web.Browser

import Path
import Path.IO

import qualified Match
import qualified Search
import qualified DevDocs
import qualified Dash
import qualified Server
import qualified Opt
import qualified Hoo
import qualified Upgrade
import qualified Config
import qualified Style
import qualified Slot
import qualified Embedded
import qualified Entry
import qualified Doc
import Utils

startGUI :: Config.T -> ConfigRoot -> Path a Dir -> Slot.T -> IO ()
startGUI config configRoot guiDir slot = do

  matchesTVar <- atomically $ newTVar ([] :: [Match.T])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  summonKey <- newSignalKey :: IO (SignalKey (IO ()))
  summonText <- atomically $ newTVar ("" :: Text)

  classMatch <- Match.defClass

  -- controls the GUI
  classController <- newClass
    [ defSignal "summon" summonKey

    , defPropertyRO' "summonText"
        (\_obj ->
          readTVarIO summonText)
    ]

  objectController <- newObject classController ()

  classContext <- newClass
    [ defPropertySigRO' "matches" matchesKey
        (\_obj -> do
          matches <- readTVarIO matchesTVar
          -- newObject :: forall tt. Class tt -> tt -> IO (ObjRef tt)
          mapM (newObject classMatch) matches)

    , defMethod' "search"
        (\_obj txt ->
          atomically $ updateTMVar (Slot.query slot) (Slot.GuiQuery $ Text.unpack txt))

    , defMethod' "setClipboard"
        (\_obj txt ->
          setClipboard . Text.unpack $ txt)

    , defMethod' "google"
        (\_obj txt ->
          google (Config.commands config) . Text.unpack $ txt)

    , defPropertyConst' "hoogleCommands"
        (\_obj ->
          return . Config.hoogleCommands $ (Config.commands config))

    , defPropertyRO' "controller"
        (\_obj ->
          return objectController)
    ]

  objectContext <- newObject classContext ()

  -- send matches to C++ side
  let sendMatches matches = do
        atomically $ writeTVar matchesTVar matches `orElse` return ()
        fireSignal matchesKey objectContext

  _searchThreadId <- Search.startThread
    config
    configRoot
    slot
    sendMatches

  -- TODO @incomplete: don't block
  _ <- forkIO . forever $ do
    atomically $ do
      query <- takeTMVar (Slot.summon slot)
      writeTVar summonText (Text.pack query)
    fireSignal summonKey objectController

  let mainQml = guiDir </> [relfile|ui/main.qml|]

  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument $ toFilePath mainQml
    , contextObject = Just $ anyObjRef objectContext
    }

  -- https://hackage.haskell.org/package/hsqml-0.3.5.0/docs/Graphics-QML-Engine.html#v:shutdownQt
  -- > It is recommended that you call this function at the end of your program ...
  shutdownQt

google :: Config.Commands -> String -> IO ()
google commands str =
  case Search.makeQuery commands str of
    Nothing ->
      return ()
    Just query -> do
      let q = Search.queryToGoogle query
      let url = "https://www.google.com/search?q=" ++ q
      fireAndForget $ openBrowser url

main :: IO ()
main = withSystemTempDir "doc-browser-gui-" $ \guiDir -> do

  opt <- Opt.get

  -- TODO @incomplete: handle the absolute/relative semantic in the type level
  configRoot <- ConfigRoot <$> getXdgDir XdgConfig (Just [reldir|doc-browser|])
  cacheRoot <- CacheRoot <$> getXdgDir XdgCache (Just [reldir|doc-browser|])

  createDirIfMissing True $ getConfigRoot configRoot
  createDirIfMissing True $ getCacheRoot cacheRoot

  config <- Config.load configRoot

  -- GUI setup
  Embedded.extractUIDirInto guiDir

  Style.createQml guiDir config
  oldQmlPath <- lookupEnv "QML2_IMPORT_PATH"
  let qmlPath = case trim <$> oldQmlPath of
        Nothing -> toFilePath guiDir
        Just "" -> toFilePath guiDir
        Just old -> old ++ ":" ++ toFilePath guiDir
  setEnv "QML2_IMPORT_PATH" qmlPath

  -- This flag is required by QtWebEngine
  -- https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html
  True <- setQtFlag QtShareOpenGLContexts True

  let withConfigLock = withLock (getConfigRoot configRoot)
  let withConfigAndCacheLock action =
        let action' = withLock (getConfigRoot configRoot) action
        in withLock (getCacheRoot cacheRoot) action'

  upgradeResult <- withConfigLock $ Upgrade.start configRoot guiDir
  case upgradeResult of
    Upgrade.Abort ->
      return ()

    Upgrade.Continue ->
      case opt of
        Opt.StartGUI logging -> do
          slot <- atomically $ Slot.empty
          _ <- forkIO $ Server.start logging config configRoot cacheRoot slot
          startGUI config configRoot guiDir slot

        Opt.InstallDevDocs ccvs ->
          withConfigLock $ DevDocs.installMany configRoot ccvs

        Opt.ListInstalledDevDocs ->
          Entry.listInstalled configRoot Doc.DevDocs

        Opt.ListRemoteDevDocs ->
          DevDocs.listRemote

        Opt.RemoveDevDocs cvs ->
          withConfigLock $ Entry.removeMany configRoot Doc.DevDocs cvs

        Opt.InstallDash collections downloadMethod ->
          withConfigLock $ Dash.installMany configRoot collections downloadMethod

        Opt.ListInstalledDash ->
          Entry.listInstalled configRoot Doc.Dash

        Opt.ListRemoteDash ->
          Dash.listRemote

        Opt.RemoveDash cvs ->
          withConfigLock $ Entry.removeMany configRoot Doc.Dash cvs

        Opt.InstallHoogle url collection ->
          withConfigAndCacheLock $ Hoo.install configRoot cacheRoot url collection

        Opt.PrintPublicAPI ->
          putStr Server.publicApiMarkdown

        Opt.PrintDefaultConfig ->
          Char8.putStr Embedded.configYaml

        Opt.PrintPort ->
          Char8.putStr . Yaml.encode . object $
            [("port", Number . fromIntegral . Config.port $ config)]
