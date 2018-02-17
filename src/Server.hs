{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (start) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Binary.Builder as Builder
import qualified Data.Map.Strict as Map
import qualified Data.String
import qualified Data.ByteString.Lazy as LBS

import System.FilePath

import Utils
import qualified Devdocs
import qualified DevdocsMeta

start port configRoot = run port (app configRoot)

app :: FilePath -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app configRoot request respond = do
  let paths = pathInfo request
  builder <- case paths of
               -- TODO @incomplete: use Devdocs.devdocs
               ("devdocs" : langver : rest) ->
                 let (language, version) = breakLangVer . Text.unpack $ langver
                 in fetchDevdocs configRoot (Text.pack language) (Text.pack version) (Text.intercalate "/" rest)
               _ -> do
                 -- TODO @incomplete: better error message or use a type safe route lib
                 return $ Builder.fromByteString $ rawPathInfo request
  let headers = [("Content-Type", "text/html; charset=utf-8")]
  respond (responseBuilder status200 headers builder)

fetchDevdocs :: FilePath -> Text -> Text -> Text -> IO Builder.Builder
fetchDevdocs configRoot language version path = do
  let filePath = joinPath
        [ configRoot
        , Devdocs.getDocFile (Text.unpack language) (Text.unpack version) (Text.unpack path)
        ]

  content <- Builder.fromLazyByteString <$> LBS.readFile filePath

  -- TODO @incomplete: handle the concatenation properly
  let begin' =
        "<html>\
        \<head>\
        \  <meta charset='utf-8'>\
        \  <link rel='stylesheet' href='https://devdocs.io/application.css'>\
        \</head>\
        \<body>\
        \  <main class='_content' role='main'>"
      pageDiv =
        case Map.lookup language DevdocsMeta.typeMap of
          Nothing ->
            "<div class='_page'>"
          Just t ->
            "<div class='_page _" ++ t ++ "'>"
      begin = Builder.fromByteString . Data.String.fromString $ begin' ++ pageDiv

  let end = Builder.fromByteString $
        "    </div>\
        \  </main>\
        \</body>\
        \</html>"

  return $ begin <> content <> end
