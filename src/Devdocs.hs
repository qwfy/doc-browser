{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devdocs
  ( loadAll
  , getDocFile
  , devdocs
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
import Utils


-- used to extract information from devdocs' index.json
data Index = Index
  { name :: String -- what will be searched
  , path :: String -- url including fragments
  } deriving (Show, Generic)

newtype IndexList = IndexList [Index]

instance Aeson.FromJSON Index

instance Aeson.FromJSON IndexList where
  parseJSON = (\(Aeson.Object object) -> do
    indices' <- object Aeson..: "entries"
    IndexList <$> Aeson.parseJSONList indices')

-- TODO @incomplete: use ADT?
devdocs = "devdocs"

-- scan devdocs' root, return languages and versions
scan :: FilePath -> IO [(String, String)]
scan docRoot = do
  let dir = docRoot </> devdocs
  entries <- listDirectory dir
  dirs <- filterM doesDirectoryExist (map (dir </>) entries)
  return $ map breakLangVer dirs


loadAll :: FilePath -> IO [Entry.T]
loadAll configRoot = do
  entries <- scan configRoot
  let loadOne = map (uncurry (load configRoot)) entries
  concat <$> sequence loadOne


load :: FilePath -> String -> String -> IO [Entry.T]
load docRoot language version = do
  let dirname = combineLangVer language version
  let indexJson = joinPath [docRoot, devdocs, dirname, "index.json"]
  bs <- LBS.readFile indexJson
  let Just (IndexList indices) = Aeson.decode' bs
  return $ map indexToEntry indices
  where
    indexToEntry (Index {name, path}) =
      Entry.T { Entry.name = name
              , Entry.path = path
              , Entry.language = language
              , Entry.version = version
              , Entry.source = devdocs
              , Entry.nameLower = C.pack $ map Data.Char.toLower name
              }

getDocFile :: String -> String -> String -> String
getDocFile language version path =
  joinPath [ devdocs
           , combineLangVer language version
           , fixPath path]

-- add extension at the appropriate place
fixPath :: String -> String
fixPath path =
  let Just uri = URI.parseRelativeReference path
      newUri = uri {URI.uriPath = URI.uriPath uri <.> ".html"}
  in URI.uriToString id newUri ""
