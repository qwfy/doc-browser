{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (start) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Binary.Builder as Builder
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.ByteString.Lazy as LBS

import System.FilePath
import System.Directory

import Data.Cache (Cache)
import qualified Data.Cache as Cache

import Control.Concurrent
import Control.Monad.Trans.Except

import Utils
import qualified Devdocs
import qualified DevdocsMeta

start :: Int -> FilePath -> FilePath -> IO ()
start port configRoot cacheRoot = do
  cache <- Cache.newCache Nothing
  loadCache cache cacheRoot
  run port (app configRoot cache)

app :: FilePath -> Cache String LBS.ByteString -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app configRoot cache request respond = do
  let paths = pathInfo request
  builder <- case paths of
               -- TODO @incomplete: use Devdocs.devdocs
               ("devdocs" : langver : rest) ->
                 let (language, version) = breakLangVer . Text.unpack $ langver
                 in fetchDevdocs configRoot cache (Text.pack language) (Text.pack version) (Text.intercalate "/" rest)
               _ ->
                 -- TODO @incomplete: better error message or use a type safe route lib
                 return $ Builder.fromByteString $ rawPathInfo request
  let headers = [("Content-Type", "text/html; charset=utf-8")]
  respond (responseBuilder status200 headers builder)

fetchDevdocs :: FilePath -> Cache String LBS.ByteString -> Text -> Text -> Text -> IO Builder.Builder
fetchDevdocs configRoot cache language version path = do
  let filePath = joinPath
        [ configRoot
        , Devdocs.getDocFile (Text.unpack language) (Text.unpack version) (Text.unpack path)
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
        case Map.lookup language DevdocsMeta.typeMap of
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
