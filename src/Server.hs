{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (start) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Binary.Builder as Builder
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Array
import Data.List.Extra
import Data.String
import qualified Data.Hash.MD5 as MD5
import Safe
import Text.Regex.PCRE

import System.FilePath
import System.Directory
import System.FileLock
import System.Posix.User

import Control.Exception

import Utils
import qualified DevDocs
import qualified DevDocsMeta
import qualified Doc
import qualified Config

start :: Config.T -> FilePath -> FilePath -> IO ()
start config configRoot cacheRoot = do
  report ["wait for server slot"]
  userId <- getRealUserID
  let lockFilePath = joinPath ["/run/user", show userId, "doc-browser/server.lock"]
  createDirectoryIfMissing True $ takeDirectory lockFilePath
  withFileLock lockFilePath Exclusive $ \_lock -> do
    report ["start new server"]
    run (Config.port config) (app configRoot cacheRoot)

app :: FilePath
    -> FilePath
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app configRoot cacheRoot request respond = do
  let paths = pathInfo request |> map Text.unpack

  let badRequest =
        return $ Builder.fromByteString $ "Unhandled URL: " <> rawPathInfo request

  builder <-
    case paths of
      (vendor : cv : rest) | vendor == show Doc.DevDocs ->
        let (collection, version) = Doc.breakCollectionVersion cv
            path = intercalate "/" rest
        in fetchDevdocs configRoot cacheRoot collection version path
      (vendor : _) | vendor == show Doc.Hoogle ->
        let path = intercalate "/" paths
        in fetchHoogle configRoot path

      ("cache" : rest) ->
        let path = joinPath $ cacheRoot:rest
        in Builder.fromLazyByteString <$> LBS.readFile path

      ("abspath" : rest) ->
        let path = joinPath $ "/":rest
        in Builder.fromLazyByteString <$> LBS.readFile path

      _ ->
        badRequest

  -- TODO @incomplete: send css and js files directly to socket instead of reading and then sending
  let mime ext
        | ext == ".css" = "text/css; charset=utf-8"
        | ext == ".js"  = "text/javascript; charset=utf-8"
        | otherwise     = "text/html; charset=utf-8"

  let contentType = maybe
        "text/html; charset=utf-8"
        (mime . takeExtension)
        (lastMay paths)

  let headers = [("Content-Type", contentType)]
  respond (responseBuilder status200 headers builder)


fetchHoogle :: FilePath -> FilePath -> IO Builder.Builder
fetchHoogle configRoot path = do
  let filePath = joinPath [configRoot, path]
  fileToRead <- do
    isFile <- doesFileExist filePath
    if isFile
      then return filePath
      else do
        let indexHtml = filePath </> "index.html"
        indexExist <- doesFileExist indexHtml
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
  hasMathJaxDist <- doesDirectoryExist distDir
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
          let localFile = fromString $ joinPath ["/abspath", tail distDir, "MathJax.js"]
          return $ before' <> "src=\"" <> localFile <> cfg <> "\"" <> after'

fetchDevdocs :: FilePath
             -> FilePath
             -> Doc.Collection
             -> Doc.Version
             -> String
             -> IO Builder.Builder
fetchDevdocs configRoot cacheRoot collection version path = do
  let filePath = joinPath
        [ configRoot
        , DevDocs.getDocFile collection version path
        ]

  content <- Builder.fromLazyByteString <$> LBS.readFile filePath

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
getCached :: FilePath -> LBS.ByteString -> String -> IO LBS.ByteString
getCached cacheRoot url' ext = do
  let url = C.unpack url'
  let basename = MD5.md5s (MD5.Str url) <.> ext
  let cachedUrl = fromString $ joinPath ["/cache", basename]
  let storage = joinPath [cacheRoot, basename]
  cached <- doesFileExist storage
  if cached
    then
      return cachedUrl
    else do
      dlRes <- try $ downloadFile' url storage :: IO (Either SomeException ())
      case dlRes of
        Right () -> do
          report ["cached url:", url, "to:", storage]
          return cachedUrl
        Left e -> do
          report ["error caching:", url, show e]
          return url'
