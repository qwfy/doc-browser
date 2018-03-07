{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}

-- what will be searched on
module Entry
  -- TODO @incomplete: expose accessor functions without exposing data constructor
  ( T(..)
  , buildUrl
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as C
import System.FilePath

import Utils

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- the choice of String is due to that Text.EditDistance only supports String
-- TODO @incomplete: use a newtype for these fields, it's quite confusing
data T = T
  { language  :: String
  , version   :: String
  , name      :: String
  , path      :: String
  , source    :: String
  , nameLower :: C.ByteString
  } deriving (Eq, Ord, Show, Generic, NFData)

-- TODO @incomplete: refactor to handle hoogle
buildUrl :: T -> Int -> Text
buildUrl T{source, language, version, path} port =
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
