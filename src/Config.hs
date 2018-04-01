{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( T(..)
  , load
  ) where

import Data.Aeson
import Data.Yaml.Config
import System.FilePath
import System.Directory

import GHC.Generics

import Utils
import Paths_doc_browser


data T = T
  { webEngineZoomFactor :: Float
  } deriving (Show, Generic)

instance FromJSON T where
  parseJSON = genericParseJSON $ defaultOptions
    {fieldLabelModifier = uppercaseFirst}

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
