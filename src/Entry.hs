{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- what will be searched on
module Entry
  ( Entry(..)
  , Searchable(..)
  , toMatches
  , loadSearchables
  , listInstalled
  , removeMany
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List

import qualified System.FilePath as FilePath

import Control.Monad

import Database.Persist.Sqlite

import Path
import Fmt

import qualified Match
import qualified Doc
import qualified Dash
import Db
import Utils

data Searchable = Searchable
  { saKey :: Key Entry
  , saNameLower :: Char8.ByteString
  , saCollection :: Doc.Collection
  , saVersionLower :: String
  , saVendor :: Doc.Vendor
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
        , entryPath]

    Doc.Dash ->
      FilePath.joinPath
        [ show Doc.Dash
        , Dash.b64EncodeCV entryCollection entryVersion
        , toFilePath Dash.extraDirs3
        , entryPath]

    Doc.Hoogle ->
      error $ "Bad vendor: " ++ show entryVendor

loadSearchables :: DbMonad [Searchable]
loadSearchables = do
  (rows :: [Entity Entry]) <- selectList [] []
  return $ map toSearchable rows
  where
    toSearchable (Entity{entityKey, entityVal=Entry{entryName, entryCollection, entryVersion, entryVendor}}) =
      Searchable
        { saKey = entityKey
        , saNameLower = Char8.pack $ map toLower entryName
        , saCollection = entryCollection
        , saVersionLower = map toLower $ show entryVersion
        , saVendor = entryVendor
        }

-- TODO @incomplete: this function has bad performance - due to the limit of the persistent library
listInstalled :: ConfigRoot -> Doc.Vendor -> IO ()
listInstalled configRoot vendor =
  case vendor of
    Doc.DevDocs -> doit
    Doc.Dash    -> doit
    Doc.Hoogle  -> fail . unwords $ ["Vendor", show vendor, "is not supported"]
  where
    doit = do
      rows <- runSqlite (dbPathText configRoot) . asSqlBackend $
        selectList [EntryVendor ==. vendor] []
      rows
        |> map (\(Entity{entityVal=e}) -> (entryCollection e, entryVersion e))
        |> nub
        |> map (\(c, v) -> Doc.combineCollectionVersion c v)
        |> sort
        |> fmt . blockListF
        |> putStr

removeMany :: ConfigRoot -> Doc.Vendor -> [(Doc.Collection, Doc.Version)] -> IO ()
removeMany configRoot vendor cvs = do
  case vendor of
    Doc.DevDocs -> doit
    Doc.Dash    -> doit
    Doc.Hoogle  -> fail . unwords $ ["Vendor", show vendor, "is not supported"]
  where
    doit = do
      report ["removing", show . length $ cvs, "docsets"]
      forM_ cvs $ \(c, v) -> do
        count <- runSqlite (dbPathText configRoot) . asSqlBackend $
          deleteWhereCount
            [ EntryVendor ==. vendor
            , EntryCollection ==. c
            , EntryVersion ==. v]
        report ["removed", show count, "entries from database"]

        vendorPart <- parseRelDir $ show vendor
        collectionPart <- getCollectionPart vendor c v
        let collectionHome = getConfigRoot configRoot </> vendorPart </> collectionPart
        report ["removing", toFilePath collectionHome]
        tryRemoveDir collectionHome

        report ["removed", Doc.combineCollectionVersion c v]

getCollectionPart vendor@Doc.Hoogle _c _v =
  -- TODO @incomplete: replace fail with throwM?
  fail . unwords $ ["Vendor", show vendor, "is not supported"]

getCollectionPart Doc.DevDocs collection version =
  parseRelDir $ Doc.combineCollectionVersion collection version

getCollectionPart Doc.Dash collection version =
  parseRelDir $ Dash.b64EncodeCV collection version
