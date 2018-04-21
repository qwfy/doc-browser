{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module DevDocsMeta
  ( downloadMany
  , printTypeMap
  , typeMap
  ) where

import GHC.Generics (Generic)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Control.Monad
import Control.Exception

import Path

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.List
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import qualified Doc
import Utils

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
  { metaName :: Doc.Collection
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
  metas <- downloadJSON metaJsonUrl
  let showMeta Meta{metaName, metaType} =
        Text.concat ["  , ([Doc.collection|", Text.pack $ show metaName, "|], \"", metaType, "\")"]
  mapM_ TextIO.putStrLn (Data.List.nub $ map showMeta metas)
  TextIO.putStrLn "  ]"

findRecent :: [Meta] -> [Doc.Collection] -> [Either String Meta]
findRecent metas = map find
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
      toStr want == toStr metaName
      where toStr x = show x |> Text.pack |> Text.toLower
    find want =
      case Data.List.find (isWanted want) mostRecent of
        Nothing ->
          Left $ unwords ["docset", show want, "is not found"]
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
downloadMany :: ConfigRoot -> [Doc.Collection] -> IO ()
downloadMany configRoot collections = do
  putStrLn "=== Docsets are provided by https://devdocs.io ==="
  unpackTo <- (getConfigRoot configRoot </>) <$> (parseRelDir $ show Doc.DevDocs)
  report ["downloading", show $ length collections, "docsets to", toFilePath unpackTo]

  metas <- downloadJSON metaJsonUrl
  let matches = findRecent metas collections
  forM_ matches (\x -> downloadOne unpackTo x `catch` reportExceptions)

  where
    downloadOne _ (Left e) = fail e
    downloadOne unpackTo (Right meta@Meta{metaName, metaRelease}) = do
      let url = toDownloadUrl meta
      let docId = Data.List.intercalate "-"
            [ show metaName
            , maybe "<no version>" Text.unpack metaRelease]
      report ["downloading", docId , "from", url]
      bs <- download url
      let collectionHome = Doc.combineCollectionVersion
            metaName
            (Doc.Version . Text.unpack $ fromMaybe "" metaRelease)
      dir <- (unpackTo </>) <$> (parseRelDir collectionHome)
      report ["unpacking", docId, "to", toFilePath dir]
      untgz bs dir
      report ["installed", docId]

typeMap :: Map.Map Doc.Collection String
typeMap = Map.fromList
  [ ([Doc.collection|Angular|], "angular")
  , ([Doc.collection|Angular.js|], "angularjs")
  , ([Doc.collection|Ansible|], "sphinx")
  , ([Doc.collection|Apache HTTP Server|], "apache")
  , ([Doc.collection|Apache Pig|], "apache_pig")
  , ([Doc.collection|Async|], "async")
  , ([Doc.collection|Babel|], "simple")
  , ([Doc.collection|Backbone.js|], "underscore")
  , ([Doc.collection|Bluebird|], "simple")
  , ([Doc.collection|Bootstrap|], "bootstrap")
  , ([Doc.collection|Bottle|], "sphinx")
  , ([Doc.collection|Bower|], "bower")
  , ([Doc.collection|C|], "c")
  , ([Doc.collection|C++|], "c")
  , ([Doc.collection|CakePHP|], "cakephp")
  , ([Doc.collection|Chai|], "chai")
  , ([Doc.collection|Chef|], "sphinx_simple")
  , ([Doc.collection|Clojure|], "clojure")
  , ([Doc.collection|CMake|], "sphinx_simple")
  , ([Doc.collection|Codeception|], "codeception")
  , ([Doc.collection|CodeceptJS|], "codeceptjs")
  , ([Doc.collection|CodeIgniter|], "sphinx")
  , ([Doc.collection|CoffeeScript|], "coffeescript")
  , ([Doc.collection|Cordova|], "cordova")
  , ([Doc.collection|Crystal|], "crystal")
  , ([Doc.collection|CSS|], "mdn")
  , ([Doc.collection|D|], "d")
  , ([Doc.collection|D3.js|], "d3")
  , ([Doc.collection|Django|], "sphinx")
  , ([Doc.collection|Docker|], "docker")
  , ([Doc.collection|Dojo|], "dojo")
  , ([Doc.collection|DOM|], "mdn")
  , ([Doc.collection|DOM Events|], "mdn")
  , ([Doc.collection|Drupal|], "drupal")
  , ([Doc.collection|Electron|], "electron")
  , ([Doc.collection|Elixir|], "elixir")
  , ([Doc.collection|Ember.js|], "ember")
  , ([Doc.collection|Erlang|], "erlang")
  , ([Doc.collection|ESLint|], "simple")
  , ([Doc.collection|Express|], "express")
  , ([Doc.collection|Falcon|], "sphinx")
  , ([Doc.collection|Fish|], "fish")
  , ([Doc.collection|Flow|], "flow")
  , ([Doc.collection|GCC|], "gnu")
  , ([Doc.collection|Git|], "git")
  , ([Doc.collection|GNU Fortran|], "gnu")
  , ([Doc.collection|Go|], "go")
  , ([Doc.collection|Godot|], "sphinx_simple")
  , ([Doc.collection|Grunt|], "grunt")
  , ([Doc.collection|Haskell|], "haskell")
  , ([Doc.collection|Haxe|], "haxe")
  , ([Doc.collection|Homebrew|], "simple")
  , ([Doc.collection|HTML|], "mdn")
  , ([Doc.collection|HTTP|], "mdn")
  , ([Doc.collection|Immutable.js|], "immutable")
  , ([Doc.collection|InfluxData|], "influxdata")
  , ([Doc.collection|Jasmine|], "jasmine")
  , ([Doc.collection|JavaScript|], "mdn")
  , ([Doc.collection|Jekyll|], "jekyll")
  , ([Doc.collection|Jest|], "jest")
  , ([Doc.collection|jQuery|], "jquery")
  , ([Doc.collection|jQuery Mobile|], "jquery")
  , ([Doc.collection|jQuery UI|], "jquery")
  , ([Doc.collection|JSDoc|], "simple")
  , ([Doc.collection|Julia|], "julia")
  , ([Doc.collection|Julia|], "sphinx_simple")
  , ([Doc.collection|Knockout.js|], "knockout")
  , ([Doc.collection|Kotlin|], "kotlin")
  , ([Doc.collection|Laravel|], "laravel")
  , ([Doc.collection|Less|], "less")
  , ([Doc.collection|Liquid|], "liquid")
  , ([Doc.collection|lodash|], "lodash")
  , ([Doc.collection|Lua|], "lua")
  , ([Doc.collection|LÖVE|], "love")
  , ([Doc.collection|Marionette.js|], "marionette")
  , ([Doc.collection|Markdown|], "markdown")
  , ([Doc.collection|Matplotlib|], "sphinx")
  , ([Doc.collection|Meteor|], "meteor")
  , ([Doc.collection|Mocha|], "mocha")
  , ([Doc.collection|Modernizr|], "modernizr")
  , ([Doc.collection|Moment.js|], "moment")
  , ([Doc.collection|Mongoose|], "mongoose")
  , ([Doc.collection|nginx|], "nginx")
  , ([Doc.collection|nginx / Lua Module|], "github")
  , ([Doc.collection|Nim|], "nim")
  , ([Doc.collection|Node.js|], "node")
  , ([Doc.collection|Nokogiri|], "rdoc")
  , ([Doc.collection|npm|], "npm")
  , ([Doc.collection|NumPy|], "sphinx")
  , ([Doc.collection|OpenJDK|], "openjdk")
  , ([Doc.collection|OpenTSDB|], "sphinx_simple")
  , ([Doc.collection|Padrino|], "rubydoc")
  , ([Doc.collection|pandas|], "sphinx")
  , ([Doc.collection|Perl|], "perl")
  , ([Doc.collection|Phalcon|], "phalcon")
  , ([Doc.collection|Phaser|], "phaser")
  , ([Doc.collection|Phoenix|], "elixir")
  , ([Doc.collection|PHP|], "php")
  , ([Doc.collection|PHPUnit|], "phpunit")
  , ([Doc.collection|PostgreSQL|], "postgres")
  , ([Doc.collection|Pug|], "pug")
  , ([Doc.collection|Python|], "sphinx")
  , ([Doc.collection|Q|], "github")
  , ([Doc.collection|Ramda|], "ramda")
  , ([Doc.collection|React|], "simple")
  , ([Doc.collection|ReactNative|], "react_native")
  , ([Doc.collection|Redis|], "redis")
  , ([Doc.collection|Redux|], "redux")
  , ([Doc.collection|Relay|], "simple")
  , ([Doc.collection|RequireJS|], "requirejs")
  , ([Doc.collection|RethinkDB|], "rethinkdb")
  , ([Doc.collection|Ruby|], "rdoc")
  , ([Doc.collection|Ruby / Minitest|], "rdoc")
  , ([Doc.collection|Ruby on Rails|], "rdoc")
  , ([Doc.collection|Rust|], "rust")
  , ([Doc.collection|Sass|], "yard")
  , ([Doc.collection|scikit-image|], "sphinx")
  , ([Doc.collection|scikit-learn|], "sphinx")
  , ([Doc.collection|Sinon.JS|], "sinon")
  , ([Doc.collection|Socket.IO|], "socketio")
  , ([Doc.collection|SQLite|], "sqlite")
  , ([Doc.collection|Statsmodels|], "sphinx")
  , ([Doc.collection|Support Tables|], "support_tables")
  , ([Doc.collection|SVG|], "mdn")
  , ([Doc.collection|Symfony|], "laravel")
  , ([Doc.collection|Tcl/Tk|], "tcl_tk")
  , ([Doc.collection|TensorFlow|], "tensorflow")
  , ([Doc.collection|Twig|], "sphinx")
  , ([Doc.collection|TypeScript|], "typescript")
  , ([Doc.collection|Underscore.js|], "underscore")
  , ([Doc.collection|Vagrant|], "vagrant")
  , ([Doc.collection|Vue.js|], "vue")
  , ([Doc.collection|Vulkan|], "vulkan")
  , ([Doc.collection|webpack|], "webpack")
  , ([Doc.collection|XSLT & XPath|], "mdn")
  , ([Doc.collection|Yarn|], "yarn")
  , ([Doc.collection|Yii|], "yii")
  ]
