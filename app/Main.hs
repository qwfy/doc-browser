{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.QML

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import System.Posix.Daemonize

import qualified Data.Text as Text
import System.Directory
import System.FilePath

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


startGUI :: FilePath -> FilePath -> IO ()
startGUI configRoot cacheRoot = do

  -- TODO @incomplete: read port from config
  let port = 7701
  _serverThreadId <- forkIO $ Server.start port configRoot cacheRoot

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
    ]

  objectContext <- newObject classContext ()

  -- send matches to C++ side
  let sendMatches matches = do
        atomically $ writeTVar matchesTVar matches `orElse` return ()
        fireSignal matchesKey objectContext

  -- TODO @incomplete: check for updates
  allEntries <- DevDocs.loadAll configRoot
  report ["number of entries:", show $ length allEntries]

  hooMay <- Hoo.findDatabase configRoot
  _searchThreadId <- Search.startThread
    (Entry.toMatch port)
    allEntries
    ((configRoot </>) <$> hooMay)
    querySlot
    sendMatches

  mainQml <- getDataFileName "ui/main.qml"

  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument mainQml
    , contextObject = Just $ anyObjRef objectContext
    }

  -- https://hackage.haskell.org/package/hsqml-0.3.5.0/docs/Graphics-QML-Engine.html#v:shutdownQt
  -- > It is recommended that you call this function at the end of your program ...
  shutdownQt

main :: IO ()
main = do

  -- This flag is required by QtWebEngine
  -- https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html
  --
  -- It is done here because the Upgrade.hs also starts a GUI, and according to
  -- https://hackage.haskell.org/package/hsqml-0.3.5.0/docs/Graphics-QML-Engine.html#v:setQtFlag
  -- > Setting flags once the Qt event loop is entered is unsupported and will also cause this function to return False.
  True <- setQtFlag QtShareOpenGLContexts True

  opt <- Opt.get

  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  cacheRoot <- getXdgDirectory XdgCache "doc-browser"

  upgradeResult <- Upgrade.startGUI configRoot
  case upgradeResult of
    Upgrade.Abort ->
      return ()

    Upgrade.Continue ->
      case opt of
        Opt.StartGUI ground ->
          let start = startGUI configRoot cacheRoot
          in case ground of
               Opt.Background -> daemonize start
               Opt.Foreground -> start

        Opt.InstallDevDocs collections ->
          DevDocsMeta.downloadMany configRoot collections

        Opt.InstallHoogle url collection ->
          Hoo.install configRoot cacheRoot url collection
