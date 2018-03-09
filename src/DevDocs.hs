{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module DevDocs
  ( loadAll
  , getDocFile
  ) where

import GHC.Generics (Generic)

import qualified Network.URI as URI
import Control.Monad (filterM)

import System.FilePath
import System.Directory

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import qualified Data.Aeson as Aeson
import qualified Data.Char

import qualified Entry
import qualified Doc


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
scan :: FilePath -> IO [(Doc.Collection, Doc.Version)]
scan configRoot = do
  let dir = configRoot </> show Doc.DevDocs
  entries <- listDirectory dir
  dirs <- filterM doesDirectoryExist (map (dir </>) entries)
  return $ map Doc.breakCollectionVersion dirs


loadAll :: FilePath -> IO [Entry.T]
loadAll configRoot = do
  entries <- scan configRoot
  let loadOne = map (uncurry (load configRoot)) entries
  concat <$> sequence loadOne


load :: FilePath -> Doc.Collection -> Doc.Version -> IO [Entry.T]
load configRoot collection version = do
  let dirName = Doc.combineCollectionVersion collection version
  let indexJson = joinPath [configRoot, show Doc.DevDocs, dirName, "index.json"]
  bs <- LBS.readFile indexJson
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
getDocFile :: Doc.Collection -> Doc.Version -> FilePath -> FilePath
getDocFile collection version path =
  joinPath [ show Doc.DevDocs
           , Doc.combineCollectionVersion collection version
           , extractPath path]

extractPath :: String -> String
extractPath path =
  let Just uri = URI.parseRelativeReference path
  in URI.uriPath uri <.> ".html"
