{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DevDocsMeta
  ( downloadMany
  , printTypeMap
  , typeMap
  ) where

import GHC.Generics (Generic)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import Control.Monad.Trans.Except

import Path

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.List
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import Utils
import qualified Doc

metaJsonUrl = "https://devdocs.io/docs.json"

-- a sample in docs.json:
--
-- {
--     "name": "Haskell",
--     "slug": "haskell~8",
--     "type": "haskell",
--     "links": {
--         "home": "https://www.haskell.org/"
--     },
--     "version": "8",
--     "release": "8.2.1",
--     "mtime": 1503760719,
--     "db_size": 26228623
-- }

data Meta = Meta
  { metaName :: Text
  , metaSlug :: Text
  , metaType :: Text
  -- some entries in the json don't have these two fields
  , metaVersion :: Maybe Text
  , metaRelease :: Maybe Text
  , metaMtime :: Integer
  } deriving (Show, Generic)

instance Aeson.FromJSON Meta where
  parseJSON = Aeson.genericParseJSON $
    Aeson.defaultOptions
      {Aeson.fieldLabelModifier = modify}
    where
      modify str =
        drop (length ("meta" :: String)) str
        |> Aeson.camelTo2 '_'

printTypeMap :: IO ()
printTypeMap = do
  metasR <- runExceptT $ getMetaJson metaJsonUrl
  case metasR of
    Left err ->
      report ["error decoding json:", err]
    Right metas -> do
      let showMeta Meta{metaName, metaType} =
            Text.concat ["  , (Doc.Collection \"", metaName, "\", \"", metaType, "\")"]
      mapM_ TextIO.putStrLn (Data.List.nub $ map showMeta metas)
      TextIO.putStrLn "  ]"

getMetaJson :: String -> ExceptT String IO [Meta]
getMetaJson url = do
  bs <- download url
  ExceptT . return $ Aeson.eitherDecode bs

findRecent :: [Meta] -> [Doc.Collection] -> [Either String Meta]
findRecent metas = map (find . Doc.getCollection)
  where
    metaSortKey Meta{metaRelease, metaVersion, metaMtime} =
      (metaRelease, metaVersion, metaMtime)
    compareMeta m1 m2 =
      compare (metaSortKey m1) (metaSortKey m2)
    mostRecent = metas
      |> Data.List.groupBy (\m1 m2 -> metaName m1 == metaName m2)
      |> map (Data.List.sortBy compareMeta)
      |> map last
    isWanted want Meta{metaName} =
      Text.toLower (Text.pack want) == Text.toLower metaName
    find want =
      case Data.List.find (isWanted want) mostRecent of
        Nothing ->
          Left $ unwords ["docset", want, "is not found"]
        Just meta ->
          Right meta

toDownloadUrl Meta{metaSlug} =
  concat ["http://dl.devdocs.io/", Text.unpack metaSlug, ".tar.gz"]

-- TODO @incomplete: exception handling
-- TODO @incomplete: untar to a temp directory
untgz :: LBS.ByteString -> Path Abs Dir -> IO ()
untgz bs filePath =
  let decompressed = GZip.decompress bs
  in Tar.unpack (toFilePath filePath) (Tar.read decompressed)

-- TODO @incomplete: multithreads and proxy
downloadMany :: Path Abs Dir -> [Doc.Collection] -> IO ()
downloadMany unpackTo' collections = do
  unpackTo <- (unpackTo' </>) <$> (parseRelDir $ show Doc.DevDocs)
  report ["downloading", show $ length collections, "docsets to", toFilePath unpackTo]

  metaJsonR <- runExceptT $ getMetaJson metaJsonUrl
  case metaJsonR of
    Left e ->
      report [e]
    Right metas ->
      let matches = findRecent metas collections
      in mapM_ (downloadOne unpackTo) matches
  where
    downloadOne _ (Left e) =
      report [e]
    downloadOne unpackTo (Right meta@Meta{metaName, metaRelease}) = do
      let url = toDownloadUrl meta
      let docId = Data.List.intercalate "-"
            [ Text.unpack metaName
            , maybe "<no version>" Text.unpack metaRelease]
      report [ "downloading"
             , docId
             , "from"
             , url ]
      result <- runExceptT $ download url
      case result of
        Left e ->
          report [e]
        Right bs -> do
          let collectionHome = Doc.combineCollectionVersion
                (Doc.Collection . Text.unpack $ metaName)
                (Doc.Version . Text.unpack $ fromMaybe "" metaRelease)
          dir <- (unpackTo </>) <$> (parseRelDir collectionHome)
          report ["unpacking", docId, "to", toFilePath dir]
          untgz bs dir
          report ["installed", docId]


-- generated with:
-- printTypeMap
typeMap :: Map.Map Doc.Collection String
typeMap = Map.fromList
  [ (Doc.Collection "Angular", "angular")
  , (Doc.Collection "Angular.js", "angularjs")
  , (Doc.Collection "Ansible", "sphinx")
  , (Doc.Collection "Apache HTTP Server", "apache")
  , (Doc.Collection "Apache Pig", "apache_pig")
  , (Doc.Collection "Async", "async")
  , (Doc.Collection "Babel", "simple")
  , (Doc.Collection "Backbone.js", "underscore")
  , (Doc.Collection "Bluebird", "simple")
  , (Doc.Collection "Bootstrap", "bootstrap")
  , (Doc.Collection "Bottle", "sphinx")
  , (Doc.Collection "Bower", "bower")
  , (Doc.Collection "C", "c")
  , (Doc.Collection "C++", "c")
  , (Doc.Collection "CakePHP", "cakephp")
  , (Doc.Collection "Chai", "chai")
  , (Doc.Collection "Chef", "sphinx_simple")
  , (Doc.Collection "Clojure", "clojure")
  , (Doc.Collection "CMake", "sphinx_simple")
  , (Doc.Collection "Codeception", "codeception")
  , (Doc.Collection "CodeceptJS", "codeceptjs")
  , (Doc.Collection "CodeIgniter", "sphinx")
  , (Doc.Collection "CoffeeScript", "coffeescript")
  , (Doc.Collection "Cordova", "cordova")
  , (Doc.Collection "Crystal", "crystal")
  , (Doc.Collection "CSS", "mdn")
  , (Doc.Collection "D", "d")
  , (Doc.Collection "D3.js", "d3")
  , (Doc.Collection "Django", "sphinx")
  , (Doc.Collection "Docker", "docker")
  , (Doc.Collection "Dojo", "dojo")
  , (Doc.Collection "DOM", "mdn")
  , (Doc.Collection "DOM Events", "mdn")
  , (Doc.Collection "Drupal", "drupal")
  , (Doc.Collection "Electron", "electron")
  , (Doc.Collection "Elixir", "elixir")
  , (Doc.Collection "Ember.js", "ember")
  , (Doc.Collection "Erlang", "erlang")
  , (Doc.Collection "ESLint", "simple")
  , (Doc.Collection "Express", "express")
  , (Doc.Collection "Falcon", "sphinx")
  , (Doc.Collection "Fish", "fish")
  , (Doc.Collection "Flow", "flow")
  , (Doc.Collection "GCC", "gnu")
  , (Doc.Collection "Git", "git")
  , (Doc.Collection "GNU Fortran", "gnu")
  , (Doc.Collection "Go", "go")
  , (Doc.Collection "Godot", "sphinx_simple")
  , (Doc.Collection "Grunt", "grunt")
  , (Doc.Collection "Haskell", "haskell")
  , (Doc.Collection "Haxe", "haxe")
  , (Doc.Collection "Homebrew", "simple")
  , (Doc.Collection "HTML", "mdn")
  , (Doc.Collection "HTTP", "mdn")
  , (Doc.Collection "Immutable.js", "immutable")
  , (Doc.Collection "InfluxData", "influxdata")
  , (Doc.Collection "Jasmine", "jasmine")
  , (Doc.Collection "JavaScript", "mdn")
  , (Doc.Collection "Jekyll", "jekyll")
  , (Doc.Collection "Jest", "jest")
  , (Doc.Collection "jQuery", "jquery")
  , (Doc.Collection "jQuery Mobile", "jquery")
  , (Doc.Collection "jQuery UI", "jquery")
  , (Doc.Collection "JSDoc", "simple")
  , (Doc.Collection "Julia", "julia")
  , (Doc.Collection "Julia", "sphinx_simple")
  , (Doc.Collection "Knockout.js", "knockout")
  , (Doc.Collection "Kotlin", "kotlin")
  , (Doc.Collection "Laravel", "laravel")
  , (Doc.Collection "Less", "less")
  , (Doc.Collection "Liquid", "liquid")
  , (Doc.Collection "lodash", "lodash")
  , (Doc.Collection "Lua", "lua")
  , (Doc.Collection "LÖVE", "love")
  , (Doc.Collection "Marionette.js", "marionette")
  , (Doc.Collection "Markdown", "markdown")
  , (Doc.Collection "Matplotlib", "sphinx")
  , (Doc.Collection "Meteor", "meteor")
  , (Doc.Collection "Mocha", "mocha")
  , (Doc.Collection "Modernizr", "modernizr")
  , (Doc.Collection "Moment.js", "moment")
  , (Doc.Collection "Mongoose", "mongoose")
  , (Doc.Collection "nginx", "nginx")
  , (Doc.Collection "nginx / Lua Module", "github")
  , (Doc.Collection "Nim", "nim")
  , (Doc.Collection "Node.js", "node")
  , (Doc.Collection "Nokogiri", "rdoc")
  , (Doc.Collection "npm", "npm")
  , (Doc.Collection "NumPy", "sphinx")
  , (Doc.Collection "OpenJDK", "openjdk")
  , (Doc.Collection "OpenTSDB", "sphinx_simple")
  , (Doc.Collection "Padrino", "rubydoc")
  , (Doc.Collection "pandas", "sphinx")
  , (Doc.Collection "Perl", "perl")
  , (Doc.Collection "Phalcon", "phalcon")
  , (Doc.Collection "Phaser", "phaser")
  , (Doc.Collection "Phoenix", "elixir")
  , (Doc.Collection "PHP", "php")
  , (Doc.Collection "PHPUnit", "phpunit")
  , (Doc.Collection "PostgreSQL", "postgres")
  , (Doc.Collection "Pug", "pug")
  , (Doc.Collection "Python", "sphinx")
  , (Doc.Collection "Q", "github")
  , (Doc.Collection "Ramda", "ramda")
  , (Doc.Collection "React", "simple")
  , (Doc.Collection "ReactNative", "react_native")
  , (Doc.Collection "Redis", "redis")
  , (Doc.Collection "Redux", "redux")
  , (Doc.Collection "Relay", "simple")
  , (Doc.Collection "RequireJS", "requirejs")
  , (Doc.Collection "RethinkDB", "rethinkdb")
  , (Doc.Collection "Ruby", "rdoc")
  , (Doc.Collection "Ruby / Minitest", "rdoc")
  , (Doc.Collection "Ruby on Rails", "rdoc")
  , (Doc.Collection "Rust", "rust")
  , (Doc.Collection "Sass", "yard")
  , (Doc.Collection "scikit-image", "sphinx")
  , (Doc.Collection "scikit-learn", "sphinx")
  , (Doc.Collection "Sinon.JS", "sinon")
  , (Doc.Collection "Socket.IO", "socketio")
  , (Doc.Collection "SQLite", "sqlite")
  , (Doc.Collection "Statsmodels", "sphinx")
  , (Doc.Collection "Support Tables", "support_tables")
  , (Doc.Collection "SVG", "mdn")
  , (Doc.Collection "Symfony", "laravel")
  , (Doc.Collection "Tcl/Tk", "tcl_tk")
  , (Doc.Collection "TensorFlow", "tensorflow")
  , (Doc.Collection "Twig", "sphinx")
  , (Doc.Collection "TypeScript", "typescript")
  , (Doc.Collection "Underscore.js", "underscore")
  , (Doc.Collection "Vagrant", "vagrant")
  , (Doc.Collection "Vue.js", "vue")
  , (Doc.Collection "Vulkan", "vulkan")
  , (Doc.Collection "webpack", "webpack")
  , (Doc.Collection "XSLT & XPath", "mdn")
  , (Doc.Collection "Yarn", "yarn")
  , (Doc.Collection "Yii", "yii")
  ]
