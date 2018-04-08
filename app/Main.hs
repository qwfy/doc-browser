{-# LANGUAGE OverloadedStrings #-}

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
import System.Directory
import System.FilePath
import System.Hclip
import System.IO.Extra
import System.Environment
import Web.Browser

import qualified Match
import qualified Search
import qualified DevDocs
import qualified DevDocsMeta
import qualified Server
import qualified Opt
import qualified Hoo
import qualified Upgrade
import qualified Config
import qualified Style
import Utils

import Paths_doc_browser

startGUI :: Config.T -> FilePath -> TMVar String -> IO ()
startGUI config configRoot summonSlot = withTempDir $ \qmlModuleDir -> do

  Style.createQml qmlModuleDir config
  oldQmlPath <- lookupEnv "QML2_IMPORT_PATH"
  let qmlPath = case trim <$> oldQmlPath of
        Nothing -> qmlModuleDir
        Just "" -> qmlModuleDir
        Just old -> old ++ ":" ++ qmlModuleDir
  setEnv "QML2_IMPORT_PATH" qmlPath

  matchesTVar <- atomically $ newTVar ([] :: [Match.T])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  summonKey <- newSignalKey :: IO (SignalKey (IO ()))
  summonText <- atomically $ newTVar ("" :: Text)

  querySlot <- atomically newEmptyTMVar

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
          atomically $ updateTMVar querySlot (Text.unpack txt))

    , defMethod' "setClipboard"
        (\_obj txt ->
          setClipboard . Text.unpack $ txt)

    , defMethod' "google"
        (\_obj txt ->
          google . Text.unpack $ txt)

    , defPropertyConst' "webEngineZoomFactor"
        (\_obj ->
          return . Text.pack . show . Config.webEngineZoomFactor $ config)

    , defPropertyRO' "controller"
        (\_obj ->
          return objectController)
    ]

  objectContext <- newObject classContext ()

  -- send matches to C++ side
  let sendMatches matches = do
        atomically $ writeTVar matchesTVar matches `orElse` return ()
        fireSignal matchesKey objectContext

  -- TODO @incomplete: check for updates
  allEntries <- DevDocs.loadAll configRoot
  report ["number of entries from DevDocs:", show $ length allEntries]

  hooMay <- Hoo.findDatabase configRoot
  _searchThreadId <- Search.startThread
    config
    configRoot
    allEntries
    ((configRoot </>) <$> hooMay)
    querySlot
    sendMatches

  -- TODO @incomplete: don't block
  _ <- forkIO . forever $ do
    atomically $ do
      query <- takeTMVar summonSlot
      writeTVar summonText (Text.pack query)
    fireSignal summonKey objectController

  mainQml <- getDataFileName "ui/main.qml"

  -- This flag is required by QtWebEngine
  -- https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html
  True <- setQtFlag QtShareOpenGLContexts True

  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument mainQml
    , contextObject = Just $ anyObjRef objectContext
    }

  -- https://hackage.haskell.org/package/hsqml-0.3.5.0/docs/Graphics-QML-Engine.html#v:shutdownQt
  -- > It is recommended that you call this function at the end of your program ...
  shutdownQt

google :: String -> IO ()
google str =
  case Search.makeQuery str of
    Nothing ->
      return ()
    Just query -> do
      let q = Search.queryToGoogle query
      let url = "https://www.google.com/search?q=" ++ q
      fireAndForget $ openBrowser url

main :: IO ()
main = do
  opt <- Opt.get

  -- TODO @incomplete: handle the absolute/relative semantic in the type level
  configRoot <- makeAbsolute =<< getXdgDirectory XdgConfig "doc-browser"
  cacheRoot <- makeAbsolute =<< getXdgDirectory XdgCache "doc-browser"

  createDirectoryIfMissing True $ joinPath [configRoot]
  createDirectoryIfMissing True $ joinPath [cacheRoot]

  config <- Config.load configRoot

  upgradeResult <- Upgrade.start configRoot
  case upgradeResult of
    Upgrade.Abort ->
      return ()

    Upgrade.Continue ->
      case opt of
        Opt.StartGUI -> do
          summonSlot <- atomically $ newEmptyTMVar
          _ <- forkIO $ Server.start config configRoot cacheRoot summonSlot
          startGUI config configRoot summonSlot

        Opt.InstallDevDocs collections ->
          DevDocsMeta.downloadMany configRoot collections

        Opt.InstallHoogle url collection ->
          Hoo.install configRoot cacheRoot url collection
