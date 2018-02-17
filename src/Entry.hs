{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- what will be searched on
module Entry
  -- TODO @incomplete: expose accessor functions without exposing data constructor
  ( T(..)
  , buildUrl
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

import Utils

-- the choice of String is due to that Text.EditDistance only supports String
data T = T
  { language :: String
  , version  :: String
  , name     :: String
  , path     :: String
  , source   :: String
  } deriving (Eq, Ord, Show)

buildUrl :: T -> Int -> Text
buildUrl (T {source, language, version, path}) port =
  let p = joinPath
        [ source
        , combineLangVer language version
        , path
        ]
  in Text.concat
      [ "http://localhost:"
      , Text.pack $ show port
      , "/"
      , Text.pack p
      ]
