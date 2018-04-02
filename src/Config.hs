{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( T(..)
  , load
  ) where

import Data.Text (Text)
import Data.Aeson
import Data.Yaml.Config
import System.FilePath
import System.Directory

import Text.Mustache

import GHC.Generics

import Utils
import Paths_doc_browser


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
  } deriving (Show, Generic)


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

load :: FilePath -> IO T
load configRoot = do
  let basename = "config.yaml"
  let userConfig = configRoot </> basename
  defaultConfig <- getDataFileName basename
  userConfigExist <- doesFileExist userConfig
  let configs =
        if userConfigExist
          then [userConfig, defaultConfig]
          else [defaultConfig]
  loadYamlSettings configs [] ignoreEnv
