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

-- TH generated
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Db
  ( migrateAll
  , DbMonad
  , dbPath

  , Entry(..)
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)

import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Data.Text as Text

import System.FilePath (FilePath)
import Path

import qualified Doc
import Utils

type DbMonad a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

share [mkPersist sqlSettings, mkMigrate "migrateAll'"] [persistLowerCase|
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

migrateAll :: Path Abs File -> IO [String]
migrateAll dbPath = do
  let action :: DbMonad [String]
      action = map Text.unpack <$> runMigrationSilent migrateAll'
  runSqlite (dbPath |> toFilePath |> Text.pack) action
