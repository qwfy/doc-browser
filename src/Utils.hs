{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( combineLangVer
  , breakLangVer
  , (|>)
  , download
  , downloadFile
  , report
  ) where

import qualified Data.Text as Text
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Lens as Lens

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Network.HTTP.Types.Status
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

download :: String -> ExceptT String IO LBS.ByteString
download url = do
  resp <- lift $ Wreq.get url
  let respStatus = resp Lens.^. Wreq.responseStatus
  if respStatus == status200
    then
      return $ resp Lens.^. Wreq.responseBody
    else
      throwE . unwords $ ["error downloading", url, show respStatus]

downloadFile :: String -> FilePath -> ExceptT String IO ()
downloadFile url saveTo = do
  bs <- download url
  lift $ LBS.writeFile saveTo bs

-- TODO @incomplete: proper logging
report strs =
  putStrLn . unwords $ strs
