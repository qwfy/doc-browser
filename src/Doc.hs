{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Doc
  ( Vendor(..)
  , Collection
  , getCollection
  , parseCollection
  , collection
  , Version(..)
  , combineCollectionVersion
  , breakCollectionVersion
  , InvalidCollection
  , hasCVSep
  ) where

import Control.Exception
import Control.Monad.Catch

import Data.Aeson
import Data.List.Extra
import qualified Data.Text as Text

import qualified System.FilePath as FilePath
import Path

import Database.Persist.Sqlite
import Database.Persist.TH

import Language.Haskell.TH.Quote
import Data.Data

import Utils


-- |Vendor of a docset
data Vendor
  = DevDocs -- ^Docset provided by devdocs.io
  | Dash    -- ^Docset provided by kapeli.com/dash
  | Hoogle  -- ^Docset provided by an archive, from which a Hoogle database can be generated
  deriving (Eq, Show, Read)

derivePersistField "Vendor"


-- |Collection of the entry
--
-- For 'DevDocs' and 'Dash', this is the language or library that this entry belongs to.
--
-- For 'Hoogle', this is the @COLLECTION@ parameter in the @doc-browser --install-hoogle@ command.
newtype Collection =
  Collection {getCollection :: String}
  deriving (Eq, Ord, Data, Typeable)

instance Show Collection where
  show (Collection x) = x

instance PersistField Collection where
  toPersistValue = PersistText . Text.pack . getCollection
  fromPersistValue (PersistText txt) =
    mapLeft (Text.pack . show) . parseCollection . Text.unpack $ txt
  fromPersistValue x =
    Left . Text.pack $ "Cannot convert from: " ++ show x

instance PersistFieldSql Collection where
  sqlType _proxy = SqlString

data InvalidCollection
  = InvalidCollection String
  deriving (Show)

instance Exception InvalidCollection

instance FromJSON Collection where
  parseJSON = withText "Collection" parse
    where
      parse txt =
        case parseCollection (Text.unpack txt) of
          Left e -> fail $ show e
          Right coll -> return coll

instance ToJSON Collection where
  toJSON (Collection coll) = String . Text.pack $ coll

cvSep = "=="

hasCVSep str = cvSep `isInfixOf` str

parseCollection :: MonadThrow m => String -> m Collection
parseCollection str =
  if hasCVSep str
    then throwM $ InvalidCollection str
    else return . Collection . Data.List.Extra.replace "/" "-" $ str

collection :: QuasiQuoter
collection = QuasiQuoter
  { quoteExp = q
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
  where
    q str = do
      coll <- parseCollection str
      dataToExpQ (const Nothing) coll

-- |Version of the collection
--
-- For 'DevDocs', this is the version of the language or the library, e.g. the @3.6.4@ as in @Python 3.6.4@, or the @1.13.0@ as in @NumPy 1.13.0@.
--
-- For 'Dash', part of this is the version of the language or the library.
--
-- For 'Hoogle', this is the version of the package as specified in their cabal file.
newtype Version =
  Version {getVersion :: String}
  deriving (Eq, Ord)

instance Show Version where
  show (Version x) = x

instance Read Version where
  readsPrec _ x = [(Version x, "")]

derivePersistField "Version"

data InvalidCollectionVersion
  = InvalidCollectionVersion String
  deriving (Show)

instance Exception InvalidCollectionVersion

combineCollectionVersion :: Collection -> Version -> String
combineCollectionVersion (Collection c) (Version v) =
  c ++ "==" ++ v

breakCollectionVersion :: MonadThrow m => Path a Dir -> m (Collection, Version)
breakCollectionVersion cv =
  toFilePath cv
    |> FilePath.dropTrailingPathSeparator
    |> FilePath.takeFileName
    |> stripInfix cvSep
    |> \case
          Nothing -> throwM $ InvalidCollectionVersion (toFilePath cv)
          Just ("", _) -> throwM $ InvalidCollectionVersion (toFilePath cv)
          Just (c', v) -> do
            c <- parseCollection c'
            return (c, Version v)
