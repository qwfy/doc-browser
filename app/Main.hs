{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.QML
import Data.Typeable (Typeable)

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import System.Posix.Daemonize

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory

import qualified Entry
import qualified Search
import qualified Devdocs
import qualified DevdocsMeta
import qualified Server
import qualified Opt
import Utils

import Paths_doc_browser


-- this is what will be displayed in the search results
-- the choice of Text is due to that HsQML cannot marshal String
data Match = Match
  { matchName     :: Text
  , matchUrl      :: Text
  , matchLanguage :: Text
  , matchVersion  :: Text
  } deriving (Eq, Show, Typeable)

entryToMatch :: Int -> Entry.T -> Match
entryToMatch port entry = Match
  { matchName     = Text.pack $ Entry.name entry
  , matchLanguage = Text.pack $ Entry.language entry
  , matchVersion  = Text.pack $ Entry.version entry
  , matchUrl      = Entry.buildUrl entry port
  }

-- class on the C++ side
defClassMatch :: IO (Class Match)
defClassMatch =
  newClass
    [ defPropertyConst' "name"
        (\obj -> return (matchName $ fromObjRef obj))

    , defPropertyConst' "url"
        (\obj -> return (matchUrl $ fromObjRef obj))

    , defPropertyConst' "language"
        (\obj -> return (matchLanguage $ fromObjRef obj))

    , defPropertyConst' "version"
        (\obj -> return (matchVersion $ fromObjRef obj))
    ]


startGUI :: FilePath -> FilePath -> IO ()
startGUI configRoot cacheRoot = do

  -- TODO @incomplete: read port from config
  let port = 7701
  _serverThreadId <- forkIO $ Server.start port configRoot cacheRoot

  matchesTVar <- atomically $ newTVar ([] :: [Match])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  querySlot <- atomically $ newEmptyTMVar

  classMatch <- defClassMatch

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
  let sendEntries entries = do
        let matches = map (entryToMatch port) entries
        atomically $ writeTVar matchesTVar matches `orElse` return ()
        fireSignal matchesKey objectContext

  -- TODO @incomplete: check for updates
  allEntries <- Devdocs.loadAll configRoot
  report ["number of entries:", show $ length allEntries]
  _searchThreadId <- Search.startThread allEntries querySlot sendEntries

  -- this flag is required by QtWebEngine
  -- https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html
  True <- setQtFlag QtShareOpenGLContexts True

  mainQml <- getDataFileName "ui/main.qml"

  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument mainQml
    , contextObject = Just $ anyObjRef objectContext
    }

  -- https://hackage.haskell.org/package/hsqml-0.3.5.0/docs/Graphics-QML-Engine.html#v:shutdownQt
  -- > It is recommended that you call this function at the end of your program ...
  shutdownQt

updateTMVar :: TMVar a -> a -> STM ()
updateTMVar slot x = do
  _ <- tryTakeTMVar slot
  putTMVar slot x


main :: IO ()
main = do
  opt <- Opt.get

  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  cacheRoot <- getXdgDirectory XdgCache "doc-browser"

  case opt of
    Opt.StartGUI ground ->
      let start = startGUI configRoot cacheRoot
      in case ground of
           Opt.Background -> daemonize start
           Opt.Foreground -> start

    Opt.InstallDevdocs languages ->
      DevdocsMeta.downloadMany configRoot languages
