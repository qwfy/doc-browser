{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Utils
  ( (|>)
  , download
  , downloadFile
  , download'
  , downloadFile'
  , report
  , updateTMVar
  , unpackXzInto
  , fireAndForget
  , DownloadError(..)
  , localTime
  , uppercaseFirst
  , lowercaseFirst
  , paragraph
  , paragraphs
  , joinDir
  , hasExtension
  , ConfigRoot(..)
  , CacheRoot(..)
  ) where

import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Lens as Lens

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Exception

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent

import Network.HTTP.Types.Status

import Data.Time.LocalTime
import Data.Time.Format
import Data.Char
import Data.Foldable

import Path

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.Lzma as Lzma

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

downloadFile :: String -> Path a File -> ExceptT String IO ()
downloadFile url saveTo = do
  bs <- download url
  lift $ LBS.writeFile (toFilePath saveTo) bs

download' :: String -> IO LBS.ByteString
download' url = do
  resp <- Wreq.get url
  let respStatus = resp Lens.^. Wreq.responseStatus
  if respStatus == status200
    then
      return $ resp Lens.^. Wreq.responseBody
    else
      throwIO $ DownloadError url (show respStatus)

downloadFile' :: String -> Path a File -> IO ()
downloadFile' url saveTo = do
  bs <- download' url
  LBS.writeFile (toFilePath saveTo) bs

-- TODO @incomplete: proper logging
report = putStrLn . unwords

updateTMVar :: TMVar a -> a -> STM ()
updateTMVar slot x = do
  _ <- tryTakeTMVar slot
  putTMVar slot x

unpackXzInto :: Path a File -> Path a Dir -> IO ()
unpackXzInto archive into = do
  bs <- LBS.readFile (toFilePath archive)
  Lzma.decompress bs
    |> Tar.read
    |> Tar.unpack (toFilePath into)

fireAndForget :: IO a -> IO ()
fireAndForget action = void $ forkIO (void action)

localTime :: IO String
localTime =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" <$> getZonedTime

uppercaseFirst :: String -> String
uppercaseFirst "" = ""
uppercaseFirst (h:t) = toUpper h : t

lowercaseFirst :: String -> String
lowercaseFirst "" = ""
lowercaseFirst (h:t) = toLower h : t

paragraph :: [String] -> String
paragraph = unwords

paragraphs :: [[String]] -> [String]
paragraphs = map paragraph

joinDir :: Path a Dir -> [Path Rel Dir] -> Path a Dir
joinDir first paths =
  foldl' (</>) first paths

hasExtension :: MonadThrow m => Path a File -> String -> m Bool
hasExtension path ext = do
  newPath <- path -<.> ext
  return $ newPath == path

newtype ConfigRoot = ConfigRoot {getConfigRoot :: Path Abs Dir}
newtype CacheRoot = CacheRoot {getCacheRoot :: Path Abs Dir}
