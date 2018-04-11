{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Server
  ( start
  , publicApiMarkdown
  ) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Binary.Builder as Builder
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Array
import Data.List.Extra
import Data.String
import qualified Data.Hash.MD5 as MD5
import Safe
import Text.Regex.PCRE

import System.FileLock
import System.Posix.User

import Path
import Path.IO
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory

import Control.Exception
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Monad.IO.Class
import Control.Lens ((&), (<>~))

import Servant
import Servant.Docs

import Utils
import qualified DevDocs
import qualified DevDocsMeta
import qualified Doc
import qualified Config
import qualified Slot
import qualified Match

start :: Config.T -> ConfigRoot -> CacheRoot -> Slot.T -> IO ()
start config configRoot cacheRoot slot = do
  -- TODO @incomplete: make the lock global
  userId <- getRealUserID
  lockFilePath <- do
    a <- parseRelDir $ show userId
    return $ [absdir|/run/user|] </> a </> [relfile|doc-browser/server.lock|]
  createDirIfMissing True $ parent lockFilePath
  report ["wait for server slot"]
  withFileLock (toFilePath lockFilePath) Exclusive $ \_lock -> do
    report ["start new server"]
    run (Config.port config) (app configRoot cacheRoot slot)

type API = PublicAPI :<|> PrivateAPI

api :: Proxy Server.API
api = Proxy

app :: ConfigRoot -> CacheRoot -> Slot.T -> Application
app configRoot cacheRoot slot =
  serve api
    (publicServer slot
     :<|> privateServer configRoot cacheRoot)


type Q = QueryParam' '[Required] "q" String

instance ToParam Q where
  toParam _ =
    DocQueryParam
      "q"
      ["os.path", "/pyos.path", "forM/hh", "etc."]
      "String to search"
      Normal

instance ToSample () where
  toSamples _ = noSamples

instance ToSample Match.T where
  toSamples _ = samples
    [ Match.T
        { Match.vendor         = "DevDocs"
        , Match.name           = "os.path"
        , Match.collection     = "Python"
        , Match.version        = "3.6.4"
        , Match.url            = "http://localhost:7701/DevDocs/Python==3.6.4/library/os.path"
        , Match.package_       = Nothing
        , Match.module_        = Nothing
        , Match.typeConstraint = Nothing}
    -- , Match.T
    --     { Match.vendor         = "DevDocs"
    --     , Match.name           = "os.PathLike"
    --     , Match.collection     = "Python"
    --     , Match.version        = "3.6.4"
    --     , Match.url            = "http://localhost:7701/DevDocs/Python==3.6.4/library/os#os.PathLike"
    --     , Match.package_       = Nothing
    --     , Match.module_        = Nothing
    --     , Match.typeConstraint = Nothing}
    ]

type SearchAPI = "search" :> Q :> Get '[JSON] [Match.T]

type PublicAPI
  =    "summon" :> Q :> Get '[JSON] ()
  :<|> SearchAPI

publicApi :: Proxy PublicAPI
publicApi = Proxy

publicApiMarkdown :: String
publicApiMarkdown =
  let docOptions = defaultDocOptions
      docIntro = DocIntro "HTTP API for doc-browser" $ paragraphs
        [ [ "You can interact with this application using HTTP requests."]
        , [ "All URL should be prefixed with `http://localhost:<port>`,"
          , "where `<port>` is 7701 if you didn't change it in your configuration."]
        , [ "If you are using Insomnia, you can import `insomnia.json`,"
          , "found at the root of this repository."]
        , [ "APIs are listed below."]
        ]
      extraInfo' = extraInfo (Proxy :: Proxy SearchAPI) $
        defAction & notes <>~ [DocNote "See Also"
          ["https://qwfy.github.io/doc-browser/doc/Match.html#t:T"]]
  in markdown $ docsWith docOptions [docIntro] extraInfo' publicApi

publicServer :: Slot.T -> Server PublicAPI
publicServer slot =
  summon
  :<|> search
  where
    summon :: String -> Servant.Handler ()
    summon queryStr = liftIO $ do
      atomically $ putTMVar (Slot.summon slot) queryStr

    search :: String -> Servant.Handler [Match.T]
    search queryStr = liftIO $ do
      resultTMVar <- atomically $ newEmptyTMVar
      atomically $ updateTMVar (Slot.query slot) (Slot.HttpQuery queryStr resultTMVar)
      atomically $ takeTMVar resultTMVar


type PrivateAPI
  =    "cache" :> Raw
  :<|> "abspath" :> Raw
  :<|> Raw

privateServer :: ConfigRoot -> CacheRoot -> Server PrivateAPI
privateServer configRoot cacheRoot =
  serveCache
  :<|> serveAbspath
  :<|> Tagged (rawServer configRoot cacheRoot)
  where
    serveCache = serveDirectoryWebApp (toFilePath cacheRoot)
    serveAbspath = serveDirectoryWebApp "/"


rawServer :: ConfigRoot -> CacheRoot -> Application
rawServer configRoot cacheRoot request respond = do
  let paths = pathInfo request |> map Text.unpack

  let badRequest =
        return $ Builder.fromByteString $ "Unhandled URL: " <> rawPathInfo request

  builder <-
    case paths of
      (vendor : cv : rest) | vendor == show Doc.DevDocs ->
        let (collection, version) = Doc.breakCollectionVersion cv
            path = intercalate "/" rest
        in parseRelFile path >>= fetchDevdocs configRoot cacheRoot collection version
      (vendor : _) | vendor == show Doc.Hoogle ->
        let path = intercalate "/" paths
        in fetchHoogle configRoot path

      _ ->
        badRequest

  -- TODO @incomplete: send css and js files directly to socket instead of reading and then sending
  let mime ext
        | ext == ".css" = "text/css; charset=utf-8"
        | ext == ".js"  = "text/javascript; charset=utf-8"
        | otherwise     = "text/html; charset=utf-8"

  let contentType' = maybe
        "text/html; charset=utf-8"
        (mime . FilePath.takeExtension)
        (lastMay paths)

  let headers = [("Content-Type", contentType')]
  respond (responseBuilder status200 headers builder)


-- TODO @incomplete: replace FilePath with Path a b?
fetchHoogle :: ConfigRoot -> FilePath -> IO Builder.Builder
fetchHoogle configRoot path = do
  let filePath = FilePath.joinPath [toFilePath configRoot, path]
  fileToRead <- do
    isFile <- Directory.doesFileExist filePath
    if isFile
      then return filePath
      else do
        let indexHtml = filePath FilePath.</> "index.html"
        indexExist <- Directory.doesFileExist indexHtml
        if indexExist
          then return indexHtml
          -- TODO @incomplete: Add this file
          else return "404.html"

  LBS.readFile fileToRead >>=
    replaceMathJax >>=
      return . Builder.fromLazyByteString

replaceMathJax :: LBS.ByteString -> IO LBS.ByteString
replaceMathJax source = do
  -- TODO @incomplete: ability to install another copy
  let distDir = "/usr/share/mathjax"
  hasMathJaxDist <- Directory.doesDirectoryExist distDir
  if not hasMathJaxDist
    then return source
    else do
      let str = "src=\"https?://.+/MathJax\\.js(\\?config=[^\"]+)\"" :: LBS.ByteString
      let regex = makeRegex str :: Regex
      case matchOnceText regex source of
        Nothing ->
          return source
        Just (before', match', after') -> do
          let (cfg, _) = match' Data.Array.! 1
          let localFile = fromString $ FilePath.joinPath ["/abspath", tail distDir, "MathJax.js"]
          return $ before' <> "src=\"" <> localFile <> cfg <> "\"" <> after'

fetchDevdocs :: ConfigRoot
             -> CacheRoot
             -> Doc.Collection
             -> Doc.Version
             -> Path Rel File
             -> IO Builder.Builder
fetchDevdocs configRoot cacheRoot collection version path = do
  filePath <- (configRoot </>) <$> DevDocs.getDocFile collection version path

  content <- Builder.fromLazyByteString <$> LBS.readFile (toFilePath filePath)

  cssUrl <- getCached cacheRoot "https://devdocs.io/application.css" "css"
  let css = "<link rel='stylesheet' href='" <> cssUrl <> "'>"

  -- TODO @incomplete: handle the concatenation properly
  let begin' =
        "<html>\
        \<head>\
        \  <meta charset='utf-8'>\
        \ " <> css <> "\
        \</head>\
        \<body>\
        \  <main class='_content' role='main'>"
      pageDiv =
        case Map.lookup collection DevDocsMeta.typeMap of
          Nothing ->
            "<div class='_page'>"
          Just t ->
            Data.String.fromString $ "<div class='_page _" ++ t ++ "'>"
      begin = Builder.fromLazyByteString $ begin' <> pageDiv

  let end = Builder.fromLazyByteString
        "    </div>\
        \  </main>\
        \</body>\
        \</html>"

  return $ begin <> content <> end


-- Since we have only one web server running on the entire OS,
-- we won't run into concurrency problems.
getCached :: CacheRoot -> LBS.ByteString -> String -> IO LBS.ByteString
getCached cacheRoot url' ext = do
  let url = LC.unpack url'
  basename <- parseRelFile (MD5.md5s (MD5.Str url)) >>= (<.> ext)
  let cachedUrl = fromString . toFilePath $ [absdir|/cache|] </> basename
  let storage = cacheRoot </> basename
  cached <- doesFileExist storage
  if cached
    then
      return cachedUrl
    else do
      dlRes <- try $ downloadFile' url storage :: IO (Either SomeException ())
      case dlRes of
        Right () -> do
          report ["cached url:", url, "to:", toFilePath storage]
          return cachedUrl
        Left e -> do
          report ["error caching:", url, show e]
          return url'
