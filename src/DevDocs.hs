{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module DevDocs
  ( loadAll
  , getDocFile
  ) where

import GHC.Generics (Generic)

import qualified Network.URI as URI
import Control.Monad.Catch

import Path
import Path.IO
import qualified System.FilePath as FilePath

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import qualified Data.Aeson as Aeson
import qualified Data.Char

import qualified Entry
import qualified Doc
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

-- scan devdocs' root, return collections and versions
scan :: ConfigRoot -> IO [(Doc.Collection, Doc.Version)]
scan configRoot = do
  dir <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
  (dirs, _) <- listDir dir
  return $ map (Doc.breakCollectionVersion . toFilePath) dirs


loadAll :: ConfigRoot -> IO [Entry.T]
loadAll configRoot = do
  entries <- scan configRoot
  let loadOne = map (uncurry (load configRoot)) entries
  concat <$> sequence loadOne


load :: ConfigRoot -> Doc.Collection -> Doc.Version -> IO [Entry.T]
load configRoot collection version = do
  let dirName = Doc.combineCollectionVersion collection version
  indexJson <- do
    a <- parseRelDir $ show Doc.DevDocs
    b <- parseRelDir $ dirName
    return $ getConfigRoot configRoot </> a </> b </> [relfile|index.json|]
  bs <- LBS.readFile . toFilePath $ indexJson
  let Just (IndexList indices) = Aeson.decode' bs
  return $ map indexToEntry indices
  where
    indexToEntry Index{name, path} =
      Entry.T { Entry.name = name
              , Entry.path = path
              , Entry.collection = collection
              , Entry.version = version
              , Entry.nameLower = C.pack $ map Data.Char.toLower name
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
