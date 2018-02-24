{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.QML
import Data.Typeable (Typeable)

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent as Concurrent

import qualified System.Posix.Daemonize as Daemonize

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory

import qualified Entry
import qualified Search
import qualified Devdocs
import qualified DevdocsMeta
import qualified Server
import qualified Opt

import Paths_doc_browser


-- this is what will be displayed in the search results
-- the choice of Text is due to that HsQML cannot marshal String
data Match = Match
  { matchName     :: Text
  , matchUrl      :: Text
  , matchLanguage :: Text
  , matchVersion  :: Text
  } deriving (Eq, Show, Typeable)

entryToMatch :: Entry.T -> Int -> Match
entryToMatch entry port = Match
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
  _threadId <- Concurrent.forkIO $ Server.start port configRoot cacheRoot

  -- TODO @incomplete: check for updates
  allEntries <- Devdocs.loadAll configRoot
  allEntriesTVar <- STM.atomically $ newTVar allEntries

  print ("number of entries", length allEntries)

  matchesTVar <- STM.atomically $ newTVar ([] :: [Match])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  searchThreadIdTm <- STM.atomically $ newEmptyTMVar

  classMatch <- defClassMatch

  classContext <- newClass
    [ defPropertySigRO' "matches" matchesKey
        (\_obj -> do
          matches <- readTVarIO matchesTVar
          -- newObject :: forall tt. Class tt -> tt -> IO (ObjRef tt)
          mapM (newObject classMatch) matches)

    , defMethod' "search"
        (\obj txt -> do
            case Search.makeQuery (Text.unpack txt) of
              Nothing -> do
                let writeOp = writeTVar matchesTVar [] `STM.orElse` return ()
                STM.atomically writeOp
                fireSignal matchesKey obj
              Just query -> do

                oldThreadIdO <- STM.atomically $ tryTakeTMVar searchThreadIdTm
                case oldThreadIdO of
                  Nothing ->
                    return ()
                  Just oldThreadId ->
                    Concurrent.killThread oldThreadId

                newThreadId <- Concurrent.forkIO
                  (do
                    -- TODO @incomplete: make this limit configurable
                    entries <- Search.search allEntriesTVar query 27
                    let matches = map (flip entryToMatch port) entries
                    let writeOp = writeTVar matchesTVar matches `STM.orElse` return ()
                    STM.atomically writeOp
                    fireSignal matchesKey obj)

                STM.atomically $ putTMVar searchThreadIdTm newThreadId
                return ()
        )
    ]

  objectContext <- newObject classContext ()

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


main :: IO ()
main = do
  opt <- Opt.get

  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  cacheRoot <- getXdgDirectory XdgCache "doc-browser"

  case opt of
    Opt.StartGUI ->
      Daemonize.daemonize $ startGUI configRoot cacheRoot

    Opt.InstallDevdocs languages ->
      DevdocsMeta.downloadMany configRoot languages
