{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Db
  ( migrateAll
  , DbMonad
  , dbPath

  , Entry(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


import System.FilePath (FilePath)
import Path
import Data.Char

import qualified Data.ByteString.Char8 as Char8

import qualified Doc
import Utils

type DbMonad a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
  name         String
  vendor       Doc.Vendor
  collection   Doc.Collection
  version      Doc.Version
  path         FilePath
  deriving     Show
|]

dbPath :: ConfigRoot -> Path Abs File
dbPath configRoot =
  getConfigRoot configRoot </> [relfile|database|]
