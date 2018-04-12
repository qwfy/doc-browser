{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Config
  ( T(..)
  , Font(..)
  , load
  ) where

import Data.Text (Text)
import Data.Aeson
import Data.Yaml.Config
import Data.Either.Utils
import qualified Data.Yaml as Yaml

import Path
import Path.IO

import GHC.Generics

import Utils
import qualified Embeded


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

load :: ConfigRoot -> IO T
load configRoot = do
  let userConfig = getConfigRoot configRoot </> [relfile|config.yaml|]
  userConfigExist <- doesFileExist userConfig
  let configs =
        if userConfigExist
          then [toFilePath userConfig]
          else []
  loadYamlSettings configs [fromRight $ Yaml.decodeEither Embeded.configYaml] ignoreEnv
