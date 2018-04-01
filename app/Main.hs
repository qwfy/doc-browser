{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.QML

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import qualified Data.Text as Text
import System.Directory
import System.FilePath
import System.Hclip
import System.Process
import Web.Browser

import qualified Entry
import qualified Match
import qualified Search
import qualified DevDocs
import qualified DevDocsMeta
import qualified Server
import qualified Opt
import qualified Hoo
import qualified Upgrade
import Utils

import Paths_doc_browser

startGUI :: Int -> FilePath -> IO ()
startGUI port configRoot = do

  matchesTVar <- atomically $ newTVar ([] :: [Match.T])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  querySlot <- atomically newEmptyTMVar

  classMatch <- Match.defClass

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
    port
    configRoot
    (Entry.toMatch port)
    allEntries
    ((configRoot </>) <$> hooMay)
    querySlot
    sendMatches

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

  -- TODO @incomplete: read port from config
  let port = 7701

  upgradeResult <- Upgrade.start configRoot
  case upgradeResult of
    Upgrade.Abort ->
      return ()

    Upgrade.Continue ->
      case opt of
        Opt.StartGUI -> do
          _ <- spawnProcess "doc-browser" ["--server", "--already-started-ok"]
          startGUI port configRoot

        Opt.StartServer startedOK -> do
          Server.start startedOK port configRoot cacheRoot

        Opt.InstallDevDocs collections ->
          DevDocsMeta.downloadMany configRoot collections

        Opt.InstallHoogle url collection ->
          Hoo.install configRoot cacheRoot url collection
