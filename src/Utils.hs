{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( combineLangVer
  , breakLangVer
  , (|>)
  ) where

import qualified Data.Text as Text
import System.FilePath

-- TODO @incomplete: replace other occurrences
combineLangVer :: String -> String -> String
combineLangVer language version =
  language ++ "==" ++ version

breakLangVer langver =
  let (a, b') = (Text.breakOn "==") . Text.pack . takeFileName $ langver
      b = Text.drop (Text.length "==") b'
  in (Text.unpack a, Text.unpack b)

infixl 0 |>
a |> f = f a
