{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( (|>)
  , download
  , downloadFile
  , download'
  , downloadFile'
  , report
  , updateTMVar
  , DownloadError(..)
  ) where

import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Lens as Lens

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Exception

import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Network.HTTP.Types.Status

infixl 0 |>
a |> f = f a

data DownloadError
  = DownloadError String String
  deriving (Show)

instance Exception DownloadError

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

download' :: String -> IO LBS.ByteString
download' url = do
  resp <- Wreq.get url
  let respStatus = resp Lens.^. Wreq.responseStatus
  if respStatus == status200
    then
      return $ resp Lens.^. Wreq.responseBody
    else
      throwIO $ DownloadError url (show respStatus)

downloadFile' :: String -> FilePath -> IO ()
downloadFile' url saveTo = do
  bs <- download' url
  LBS.writeFile saveTo bs

-- TODO @incomplete: proper logging
report = putStrLn . unwords

updateTMVar :: TMVar a -> a -> STM ()
updateTMVar slot x = do
  _ <- tryTakeTMVar slot
  putTMVar slot x
