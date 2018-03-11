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
import qualified Data.String
import qualified Data.ByteString.Lazy as LBS
import Data.List.Extra
import Safe

import System.FilePath
import System.Directory

import Data.Cache (Cache)
import qualified Data.Cache as Cache

import Control.Concurrent
import Control.Monad.Trans.Except

import Utils
import qualified DevDocs
import qualified DevDocsMeta
import qualified Doc

start :: Int -> FilePath -> FilePath -> IO ()
start port configRoot cacheRoot = do
  cache <- Cache.newCache Nothing
  loadCache cache cacheRoot
  run port (app configRoot cache)

app :: FilePath
    -> Cache String LBS.ByteString
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app configRoot cache request respond = do
  let paths = pathInfo request |> map Text.unpack

  let badRequest =
        return $ Builder.fromByteString $ "Unhandled URL: " <> rawPathInfo request

  builder <-
    case paths of
      (vendor : cv : rest) | vendor == show Doc.DevDocs ->
        let (collection, version) = Doc.breakCollectionVersion cv
            path = intercalate "/" rest
        in fetchDevdocs configRoot cache collection version path
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
  Builder.fromLazyByteString <$> LBS.readFile fileToRead


fetchDevdocs :: FilePath
             -> Cache String LBS.ByteString
             -> Doc.Collection
             -> Doc.Version
             -> String
             -> IO Builder.Builder
fetchDevdocs configRoot cache collection version path = do
  let filePath = joinPath
        [ configRoot
        , DevDocs.getDocFile collection version path
        ]

  content <- Builder.fromLazyByteString <$> LBS.readFile filePath

  maybeCss <- Cache.lookup cache devdocsApplicationCssUrl
  let css = case maybeCss of
        Nothing ->
          "<link rel='stylesheet' href='" <> Data.String.fromString devdocsApplicationCssUrl <> "'>"
        Just bs ->
          "<style>" <> bs <> "</style>"

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


loadCache :: Cache String LBS.ByteString -> FilePath -> IO ()
loadCache cache cacheRoot = do
  _ <- forkIO $ mapM_ (cacheOne cache) caches
  return ()
  where
    caches =
      [ (devdocsApplicationCssUrl, joinPath [cacheRoot, "devdocs-application.css"])
      ]

cacheOne :: Cache String LBS.ByteString -> (String, FilePath) -> IO ()
cacheOne cache (url, saveTo) = do
  exist <- doesFileExist saveTo
  if exist
    then
      cacheIt
    else do
      dlRes <- runExceptT (downloadFile url saveTo)
      case dlRes of
        Left e ->
          report ["error downloading", url, saveTo, e]
        Right () ->
          cacheIt
  where
    cacheIt = do
      LBS.readFile saveTo >>= Cache.insert cache url
      report ["cached", url, saveTo]

devdocsApplicationCssUrl :: String
devdocsApplicationCssUrl = "https://devdocs.io/application.css"
