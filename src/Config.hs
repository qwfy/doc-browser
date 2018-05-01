{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
  ( T(..)
  , Font(..)
  , load
  , Commands
  , Command(..)
  , Abbr
  , makeAbbr
  , LowerCasePrefix
  , makeLcp
  , getLcp
  , hoogleCommands
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Data.Yaml.Config
import Data.Either.Utils
import qualified Data.Yaml as Yaml
import Data.Hashable (Hashable)
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Path
import Path.IO

import GHC.Generics

import Utils
import qualified Embedded
import qualified Doc


data T = T
  { webEngineZoomFactor :: Float
  , port :: Int

  , inputFont           :: Font
  , matchFontMain       :: Font
  , matchFontMainHoogle :: Font
  , matchFontMeta       :: Font
  , matchFontVersion    :: Font
  , matchFontShortcut   :: Font

  , matchBgColorNormal   :: Text
  , matchBgColorSelected :: Text
  , matchFgColorNormal   :: Text
  , matchFgColorSelected :: Text
  , matchFgColorLight    :: Text
  , inputBorderColor     :: Text

  , commands :: Commands
  , defaultCommands :: Commands
  } deriving (Show, Generic)

type Commands = Map Abbr Command


instance FromJSON T where
  parseJSON = genericParseJSON $ defaultOptions
    {fieldLabelModifier = uppercaseFirst}

instance ToJSON T

data Font = Font
  { family :: Text
  , pointSize :: Float
  } deriving (Show, Generic)

instance FromJSON Font where
  parseJSON = genericParseJSON $ defaultOptions
    {fieldLabelModifier = uppercaseFirst}

instance ToJSON Font

newtype LowerCasePrefix = LowerCasePrefix {getLcp :: String}
  deriving (Eq)

instance Show LowerCasePrefix where
  show (LowerCasePrefix str) = str

instance FromJSON LowerCasePrefix where
  parseJSON = withText "LowerCasePrefix" $ \txt -> do
    makeLcp (Text.unpack txt) |> return

makeLcp str = map toLower str |> LowerCasePrefix

instance ToJSON LowerCasePrefix where
  toJSON (LowerCasePrefix str) = String . Text.pack $ str



newtype Abbr = Abbr {getAbbr :: Text}
  deriving (Eq, Ord, Hashable, FromJSONKey, ToJSONKey)

instance Show Abbr where
  show (Abbr txt) = Text.unpack txt

instance FromJSON Abbr where
  parseJSON = withText "Abbr" $ \txt -> do
    case Text.unpack txt of
      [a, b] ->
        return $ makeAbbr a b
      str ->
        fail . unwords $ ["Cannot convert", str, "to an Abbr"]

instance ToJSON Abbr where
  toJSON (Abbr txt) = String txt

makeAbbr a b = Abbr . Text.pack $ [a, b]


data Command
  = LimitToDevDocs Doc.Collection LowerCasePrefix
  | LimitToDash Doc.Collection LowerCasePrefix
  | HoogleLatest
  deriving (Eq, Show)

hoogleCommands :: Commands -> [Text]
hoogleCommands commands =
  Map.toList commands
    |> filter (\(_abbr, c) -> c == HoogleLatest)
    |> map (getAbbr . fst)

instance FromJSON Command where
  parseJSON = withArray "Command" $ \arr -> do
    case Vector.toList arr of
      -- TODO @incomplete: exhaustive check
      ["LimitToDevDocs", coll', lcp'] -> do
        coll <- parseJSON coll'
        lcp <- parseJSON lcp'
        return $ LimitToDevDocs coll lcp

      ["LimitToDash", coll', lcp'] -> do
        coll <- parseJSON coll'
        lcp <- parseJSON lcp'
        return $ LimitToDash coll lcp

      ["HoogleLatest"] -> do
        return HoogleLatest

      xs ->
        fail $ "Bad command: " ++ show xs


instance ToJSON Command where
  toJSON (LimitToDevDocs coll lcp) = Array . Vector.fromList $
    [String "LimitToDevDocs", toJSON coll, toJSON lcp]

  toJSON (LimitToDash coll lcp) = Array . Vector.fromList $
    [String "LimitToDash", toJSON coll, toJSON lcp]

  toJSON HoogleLatest = Array . Vector.fromList $
    [String "HoogleLatest"]


load :: ConfigRoot -> IO T
load configRoot = do
  let userConfig = getConfigRoot configRoot </> [relfile|config.yaml|]
  userConfigExist <- doesFileExist userConfig
  let configs =
        if userConfigExist
          then [toFilePath userConfig]
          else []
  t <- loadYamlSettings configs [fromRight $ Yaml.decodeEither Embedded.configYaml] ignoreEnv
  return $ t{ commands = Map.union (commands t) (defaultCommands t)
            , defaultCommands = Map.empty
            }
