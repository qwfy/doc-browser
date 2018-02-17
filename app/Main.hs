{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.QML
import Data.Typeable (Typeable)

import Control.Concurrent.STM.TVar
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent as Concurrent

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory

import qualified Entry
import qualified Search
import qualified Devdocs
import qualified Server


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


main :: IO ()
main = do

  configRoot <- getXdgDirectory XdgConfig "doc-browser"

  -- TODO @incomplete: read port from config
  let port = 7701
  _threadId <- Concurrent.forkOS $ Server.start port configRoot

  -- TODO @incomplete: check for updates
  allEntries <- Devdocs.loadAll configRoot

  matchesTVar <- STM.atomically $ newTVar ([] :: [Match])

  -- newSignalKey :: SignalSuffix p => IO (SignalKey p)
  -- instance SignalSuffix (IO ())
  matchesKey <- newSignalKey :: IO (SignalKey (IO ()))

  classMatch <- defClassMatch

  classContext <- newClass
    [ defPropertySigRO' "matches" matchesKey
        (\_obj -> do
          matches <- readTVarIO matchesTVar
          -- newObject :: forall tt. Class tt -> tt -> IO (ObjRef tt)
          mapM (newObject classMatch) matches)

    , defMethod' "search"
        (\obj txt -> do
            case Search.makeQuery txt of
              Nothing -> do
                let writeOp = writeTVar matchesTVar [] `STM.orElse` return ()
                STM.atomically writeOp
                fireSignal matchesKey obj
              Just query -> do
                _threadId <- Concurrent.forkIO
                  (do
                    -- TODO @incomplete: make this limit configurable
                    entries <- Search.search allEntries query 27
                    let matches = map (flip entryToMatch port) entries
                    let writeOp = writeTVar matchesTVar matches `STM.orElse` return ()
                    STM.atomically writeOp
                    fireSignal matchesKey obj)
                return ()
        )
    ]

  objectContext <- newObject classContext ()

  -- this flag is required by QtWebEngine
  -- https://doc.qt.io/qt-5/qml-qtwebengine-webengineview.html
  True <- setQtFlag QtShareOpenGLContexts True

  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument "ui/main.qml"
    , contextObject = Just $ anyObjRef objectContext
    }
