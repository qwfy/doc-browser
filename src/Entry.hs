{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- what will be searched on
module Entry
  -- TODO @incomplete: expose accessor functions without exposing data constructor
  ( T(..)
  , toMatch
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as C

import qualified System.FilePath as FilePath

import qualified Match
import qualified Doc

data T = T
  { collection :: Doc.Collection
  , version    :: Doc.Version
  , name       :: String
  -- file path relative to vendor's root
  -- TODO @incomplete: handle this semantic safely
  , path       :: FilePath
  , nameLower  :: C.ByteString
  } deriving (Eq)

instance Ord T where
  compare a b = compare
    (name a, version a)
    (name b, version b)

toMatch :: (String -> Text) -> T -> Match.T
toMatch prefixHost entry = Match.T
  { Match.name       = Text.pack $ name entry
  , Match.collection = Text.pack . Doc.getCollection . collection $ entry
  , Match.version    = Text.pack . Doc.getVersion . version $ entry
  , Match.url        = prefixHost $ buildUrl entry
  , Match.vendor     = Text.pack . show $ Doc.DevDocs

  , Match.package_       = Nothing
  , Match.module_        = Nothing
  , Match.typeConstraint = Nothing
  }

buildUrl :: T -> String
buildUrl T {collection, version, Entry.path = entryPath} =
  FilePath.joinPath
    [ show Doc.DevDocs
    , Doc.combineCollectionVersion collection version
    , entryPath
    ]
