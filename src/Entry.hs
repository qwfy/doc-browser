{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- what will be searched on
module Entry
  ( Entry(..)
  , Searchable(..)
  , toMatches
  , loadSearchables
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe

import qualified System.FilePath as FilePath

import Database.Persist.Sqlite

import Path

import qualified Match
import qualified Doc
import qualified Dash
import Db

data Searchable = Searchable
  { saKey :: Key Entry
  , saNameLower :: Char8.ByteString
  , saCollection :: Doc.Collection
  }

toMatches :: (String -> Text) -> [Searchable] -> DbMonad [Match.T]
toMatches prefixHost searchables = do
  let keys = map saKey searchables
  rows <- getMany keys
  return $ map (addExtra rows) searchables
  where
    addExtra rows (Searchable{saKey}) =
      let entry = fromJust $ Map.lookup saKey rows
      in toMatch entry
    toMatch entry =
      Match.T
      { Match.name       = Text.pack $ entryName entry
      , Match.collection = Text.pack . Doc.getCollection . entryCollection $ entry
      , Match.version    = Text.pack . Doc.getVersion . entryVersion $ entry
      , Match.url        = prefixHost $ buildUrl entry
      , Match.vendor     = Text.pack . show $ Doc.DevDocs

      , Match.package_       = Nothing
      , Match.module_        = Nothing
      , Match.typeConstraint = Nothing
      }

buildUrl :: Entry -> String
buildUrl Entry {entryVendor, entryCollection, entryVersion, entryPath} =
  case entryVendor of
    Doc.DevDocs ->
      FilePath.joinPath
        [ show Doc.DevDocs
        , Doc.combineCollectionVersion entryCollection entryVersion
        , entryPath
        ]
    Doc.Dash ->
      FilePath.joinPath
        [ show Doc.Dash
        , Dash.b64EncodeCV entryCollection entryVersion
        , toFilePath Dash.extraDirs3
        , entryPath
        ]
    _ ->
      error $ "Bad vendor: " ++ show entryVendor

loadSearchables :: DbMonad [Searchable]
loadSearchables = do
  (rows :: [Entity Entry]) <- selectList [] []
  return $ map toSearchable rows
  where
    toSearchable (Entity{entityKey, entityVal=Entry{entryName, entryCollection}}) =
      Searchable
        { saKey = entityKey
        , saNameLower = Char8.pack $ map toLower entryName
        , saCollection = entryCollection
        }
