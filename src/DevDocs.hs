{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DevDocs
  ( getDocFile
  , installMany
  , insertToDb
  , listRemote
  ) where

import GHC.Generics (Generic)

import qualified Network.URI as URI
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Path
import qualified System.FilePath as FilePath

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.List.Extra
import Data.Maybe

import Database.Persist.Sqlite
import Fmt

import qualified Entry
import qualified Doc
import qualified Db
import qualified DevDocsMeta
import Utils


-- used to extract information from devdocs' index.json
data Index = Index
  { name :: String -- what will be searched
  , path :: String -- url including fragments
  } deriving (Show, Generic)

newtype IndexList = IndexList [Index]

instance Aeson.FromJSON Index

instance Aeson.FromJSON IndexList where
  parseJSON = Aeson.withObject "IndexDotJson" $ \object -> do
    indices' <- object Aeson..: "entries"
    IndexList <$> Aeson.parseJSONList indices'

listRemote :: IO ()
listRemote = do
  metas <- downloadJSON DevDocsMeta.metaJsonUrl
  metas
    |> map (\m -> Doc.combineCollectionVersion
        (DevDocsMeta.metaName m)
        (maybe (Doc.Version "") (Doc.Version . Text.unpack) $ DevDocsMeta.metaRelease m))
    |> nub
    |> blockListF
    |> putStr . fmt

-- TODO @incomplete: multithreads and proxy
-- TODO @incomplete: install to a UUID dir?
installMany :: ConfigRoot -> [Doc.Collection] -> IO ()
installMany configRoot collections = do
  putStrLn "=== Docsets are provided by https://devdocs.io ==="

  unpackTo <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
  report ["downloading", show $ length collections, "docsets to", toFilePath unpackTo]

  metas <- downloadJSON DevDocsMeta.metaJsonUrl
  let matches = DevDocsMeta.findRecent metas collections
  forM_ matches (\x -> installOne unpackTo x `catch` reportExceptions)

  where
    -- TODO @incomplete: check already installed?
    installOne _ (Left e) = fail e
    installOne unpackTo (Right meta@DevDocsMeta.Meta{DevDocsMeta.metaName=collection, DevDocsMeta.metaRelease}) = do
      let url = DevDocsMeta.toDownloadUrl meta
      let docId = intercalate "-"
            [ show collection
            , maybe "<no version>" Text.unpack metaRelease]
      report ["downloading", docId , "from", url]
      bs <- download url
      let version = Doc.Version . Text.unpack $ fromMaybe "" metaRelease
      let collectionHome = Doc.combineCollectionVersion collection version
      dir <- (unpackTo </>) <$> (parseRelDir collectionHome)
      report ["unpacking", docId, "to", toFilePath dir]
      unpackTgzInto bs dir
      insertToDb configRoot collection version (dir </> [relfile|index.json|])
      report ["installed", docId]

insertToDb :: ConfigRoot -> Doc.Collection -> Doc.Version -> Path Abs File -> IO ()
insertToDb configRoot collection version indexJson = do
  let action :: Db.DbMonad ()
      action = do
        bs <- liftIO . LBS.readFile . toFilePath $ indexJson
        let Just (IndexList indices) = Aeson.decode' bs
        let entries = map indexToEntry indices
        insertMany_ entries
  runSqlite (Db.dbPath configRoot |> toFilePath |> Text.pack) action
  where
    indexToEntry Index{name, path} =
      Entry.Entry { Entry.entryName = name
                  , Entry.entryVendor = Doc.DevDocs
                  , Entry.entryCollection = collection
                  , Entry.entryVersion = version
                  , Entry.entryPath = path
                  }

-- TODO @incomplete: type signature (FilePath) is wrong
getDocFile :: MonadThrow m => Doc.Collection -> Doc.Version -> Path Rel File -> m (Path Rel File)
getDocFile collection version path = do
  dd <- parseRelDir $ show Doc.DevDocs
  cv <- parseRelDir $ Doc.combineCollectionVersion collection version
  fp <- parseRelFile $ extractPath path
  return $ dd </> cv </> fp

extractPath :: Path Rel File -> String
extractPath path =
  let Just uri = URI.parseRelativeReference (toFilePath path)
  in URI.uriPath uri FilePath.<.> ".html"
