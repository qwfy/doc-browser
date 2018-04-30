{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils
  ( (|>)
  , download
  , downloadJSON
  , downloadFile
  , report
  , updateTMVar
  , unpackXzInto
  , unpackTgzInto
  , systemUnpackTgzInto
  , fireAndForget
  , localTime
  , uppercaseFirst
  , lowercaseFirst
  , paragraph
  , paragraphs
  , joinDir
  , hasExtension
  , ConfigRoot(..)
  , CacheRoot(..)
  , tryRemoveFile
  , tryRemoveDir
  , withTempFilePath
  , extractAp
  , reportExceptions
  , mapLeft
  , withLock
  , (<//>)
  , (<..>)
  ) where

import Network.Wreq
import qualified Data.ByteString.Lazy as LBS
import Control.Lens ((^.))

import Control.Monad
import Control.Monad.Catch
import Control.Exception

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent

import Network.HTTP.Types.Status

import System.IO.Error
import System.FileLock
import qualified System.FilePath as FilePath
import System.Process

import Data.Aeson
import Data.Time.LocalTime
import Data.Time.Format
import Data.Char
import Data.Foldable
import Data.Either.Combinators

import Path
import Path.IO

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Lzma as Lzma

infixl 0 |>
a |> f = f a

data DownloadError
  = DownloadError String String
  deriving (Show)

instance Exception DownloadError

download :: String -> IO LBS.ByteString
download url = do
  resp <- get url
  let respStatus = resp ^. responseStatus
  if respStatus == status200
    then return $ resp ^. responseBody
    else throwIO $ DownloadError url (show respStatus)

downloadJSON :: FromJSON a => String -> IO a
downloadJSON url = do
  bs <- download url
  case eitherDecode' bs of
    Left err -> throwIO $ DownloadError url err
    Right a -> return a

downloadFile :: String -> Path a File -> IO ()
downloadFile url saveTo = do
  bs <- download url
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

-- TODO @incomplete: exception handling
-- TODO @incomplete: untar to a temp directory
unpackTgzInto :: LBS.ByteString -> Path Abs Dir -> IO ()
unpackTgzInto bs filePath =
  let decompressed = GZip.decompress bs
  in Tar.unpack (toFilePath filePath) (Tar.read decompressed)

systemUnpackTgzInto :: Path Abs File -> Path Abs Dir -> IO ()
systemUnpackTgzInto tgz dest = do
  createDirIfMissing True dest
  callProcess "tar" ["-C", toFilePath dest, "-xzf", toFilePath tgz]

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

tryRemoveFile :: Path a File -> IO ()
tryRemoveFile path = System.IO.Error.catchIOError (removeFile path) $
    \e -> unless (isDoesNotExistError e) $ ioError e

tryRemoveDir :: Path a Dir -> IO ()
tryRemoveDir path = System.IO.Error.catchIOError (removeDirRecur path) $
    \e -> unless (isDoesNotExistError e) $ ioError e

extractAp :: (a -> b -> IO c) -> IO a -> b -> IO c
extractAp f ma b = do
  a <- ma
  f a b

reportExceptions :: SomeException -> IO ()
reportExceptions e = report [show e]

withLock :: Path Abs Dir -> IO a -> IO a
withLock dir action = do
  let lockFile = toFilePath $ dir </> [relfile|lock|]
  hasResult <- withTryFileLock lockFile Exclusive (const action)
  case hasResult of
    Nothing ->
      fail $ "Cannot acquire lock: " ++ lockFile
    Just a ->
      return a

(<//>) = (FilePath.</>)
(<..>) = (FilePath.<.>)

withTempFilePath :: String -> (Path Abs File -> IO a) -> IO a
withTempFilePath template f = do
  withSystemTempDir template $ \dir ->
    let filePath = dir </> [relfile|file|]
    in f filePath
