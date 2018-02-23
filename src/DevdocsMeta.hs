{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DevdocsMeta
  ( downloadMany
  , printTypeMap
  , typeMap
  ) where

import GHC.Generics (Generic)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import Control.Monad.Trans.Except

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.List
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import System.FilePath

import qualified Devdocs
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
  { metaName:: Text
  , metaSlug:: Text
  , metaType:: Text
  -- TODO @incomplete: add links field
  -- some entries in the json don't have these two fields
  , metaVersion:: Maybe Text
  , metaRelease:: Maybe Text
  , metaMtime:: Integer
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
      let showMeta (Meta {metaName, metaType}) =
            Text.concat ["  , (\"", metaName, "\", \"", metaType, "\")"]
      mapM_ TextIO.putStrLn (Data.List.nub $ map showMeta metas)
      TextIO.putStrLn "  ]"

getMetaJson :: String -> ExceptT String IO [Meta]
getMetaJson url = do
  bs <- download url
  ExceptT . return $ Aeson.eitherDecode bs

findRecent :: [Meta] -> [String] -> [Either String Meta]
findRecent metas wants =
  map find wants
  where
    metaSortKey (Meta {metaRelease, metaVersion, metaMtime}) =
      (metaRelease, metaVersion, metaMtime)
    compareMeta m1 m2 =
      compare (metaSortKey m1) (metaSortKey m2)
    mostRecent = metas
      |> Data.List.groupBy (\m1 m2 -> metaName m1 == metaName m2)
      |> map (Data.List.sortBy compareMeta)
      |> map last
    isWanted want (Meta {metaName}) =
      (Text.toLower $ Text.pack want) == Text.toLower metaName
    find want =
      case Data.List.find (isWanted want) mostRecent of
        Nothing ->
          Left $ unwords [want, "is not found"]
        Just meta ->
          Right meta

toDownloadUrl (Meta {metaSlug}) =
  concat ["http://dl.devdocs.io/", Text.unpack metaSlug, ".tar.gz"]

-- TODO @incomplete: exception handling
-- TODO @incomplete: untar to a temp directory
untgz :: LBS.ByteString -> FilePath -> IO ()
untgz bs filePath =
  let decompressed = GZip.decompress bs
  in Tar.unpack filePath (Tar.read decompressed)

-- TODO @incomplete: multithreads and proxy
downloadMany :: FilePath -> [String] -> IO ()
downloadMany unpackTo' wants = do
  report ["downloading", show $ length wants, "devdocs to", unpackTo]

  metaJsonR <- runExceptT $ getMetaJson metaJsonUrl
  case metaJsonR of
    Left e ->
      report [e]
    Right metas ->
      let matches = findRecent metas wants
      in mapM_ downloadOne matches
  where
    unpackTo = joinPath [unpackTo', Devdocs.devdocs]
    downloadOne (Left e) = do
      report [e]
    downloadOne (Right meta@(Meta {metaName, metaRelease})) = do
      let url = toDownloadUrl meta
      report ["downloading", Text.unpack metaName, url]
      result <- runExceptT $ download url
      case result of
        Left e ->
          report [e]
        Right bs -> do
          let langHome = combineLangVer
                (Text.unpack metaName)
                (Text.unpack $ fromMaybe "" metaRelease)
          let dir = joinPath [unpackTo, langHome]
          report ["unpacking to", dir]
          untgz bs dir
          report ["downloaded", Text.unpack metaName]


-- generated with:
-- printTypeMap
typeMap :: Map.Map Text String
typeMap = Map.fromList
  [ ("Angular", "angular")
  , ("Angular.js", "angularjs")
  , ("Ansible", "sphinx")
  , ("Apache HTTP Server", "apache")
  , ("Apache Pig", "apache_pig")
  , ("Async", "async")
  , ("Babel", "simple")
  , ("Backbone.js", "underscore")
  , ("Bluebird", "simple")
  , ("Bootstrap", "bootstrap")
  , ("Bottle", "sphinx")
  , ("Bower", "bower")
  , ("C", "c")
  , ("C++", "c")
  , ("CakePHP", "cakephp")
  , ("Chai", "chai")
  , ("Chef", "sphinx_simple")
  , ("Clojure", "clojure")
  , ("CMake", "sphinx_simple")
  , ("Codeception", "codeception")
  , ("CodeceptJS", "codeceptjs")
  , ("CodeIgniter", "sphinx")
  , ("CoffeeScript", "coffeescript")
  , ("Cordova", "cordova")
  , ("Crystal", "crystal")
  , ("CSS", "mdn")
  , ("D", "d")
  , ("D3.js", "d3")
  , ("Django", "sphinx")
  , ("Docker", "docker")
  , ("Dojo", "dojo")
  , ("DOM", "mdn")
  , ("DOM Events", "mdn")
  , ("Drupal", "drupal")
  , ("Electron", "electron")
  , ("Elixir", "elixir")
  , ("Ember.js", "ember")
  , ("Erlang", "erlang")
  , ("ESLint", "simple")
  , ("Express", "express")
  , ("Falcon", "sphinx")
  , ("Fish", "fish")
  , ("Flow", "flow")
  , ("GCC", "gnu")
  , ("Git", "git")
  , ("GNU Fortran", "gnu")
  , ("Go", "go")
  , ("Godot", "sphinx_simple")
  , ("Grunt", "grunt")
  , ("Haskell", "haskell")
  , ("Haxe", "haxe")
  , ("Homebrew", "simple")
  , ("HTML", "mdn")
  , ("HTTP", "mdn")
  , ("Immutable.js", "immutable")
  , ("InfluxData", "influxdata")
  , ("Jasmine", "jasmine")
  , ("JavaScript", "mdn")
  , ("Jekyll", "jekyll")
  , ("Jest", "jest")
  , ("jQuery", "jquery")
  , ("jQuery Mobile", "jquery")
  , ("jQuery UI", "jquery")
  , ("JSDoc", "simple")
  , ("Julia", "julia")
  , ("Julia", "sphinx_simple")
  , ("Knockout.js", "knockout")
  , ("Kotlin", "kotlin")
  , ("Laravel", "laravel")
  , ("Less", "less")
  , ("Liquid", "liquid")
  , ("lodash", "lodash")
  , ("Lua", "lua")
  , ("LÖVE", "love")
  , ("Marionette.js", "marionette")
  , ("Markdown", "markdown")
  , ("Matplotlib", "sphinx")
  , ("Meteor", "meteor")
  , ("Mocha", "mocha")
  , ("Modernizr", "modernizr")
  , ("Moment.js", "moment")
  , ("Mongoose", "mongoose")
  , ("nginx", "nginx")
  , ("nginx / Lua Module", "github")
  , ("Nim", "nim")
  , ("Node.js", "node")
  , ("Nokogiri", "rdoc")
  , ("npm", "npm")
  , ("NumPy", "sphinx")
  , ("OpenJDK", "openjdk")
  , ("OpenTSDB", "sphinx_simple")
  , ("Padrino", "rubydoc")
  , ("pandas", "sphinx")
  , ("Perl", "perl")
  , ("Phalcon", "phalcon")
  , ("Phaser", "phaser")
  , ("Phoenix", "elixir")
  , ("PHP", "php")
  , ("PHPUnit", "phpunit")
  , ("PostgreSQL", "postgres")
  , ("Pug", "pug")
  , ("Python", "sphinx")
  , ("Q", "github")
  , ("Ramda", "ramda")
  , ("React", "simple")
  , ("ReactNative", "react_native")
  , ("Redis", "redis")
  , ("Redux", "redux")
  , ("Relay", "simple")
  , ("RequireJS", "requirejs")
  , ("RethinkDB", "rethinkdb")
  , ("Ruby", "rdoc")
  , ("Ruby / Minitest", "rdoc")
  , ("Ruby on Rails", "rdoc")
  , ("Rust", "rust")
  , ("Sass", "yard")
  , ("scikit-image", "sphinx")
  , ("scikit-learn", "sphinx")
  , ("Sinon.JS", "sinon")
  , ("Socket.IO", "socketio")
  , ("SQLite", "sqlite")
  , ("Statsmodels", "sphinx")
  , ("Support Tables", "support_tables")
  , ("SVG", "mdn")
  , ("Symfony", "laravel")
  , ("Tcl/Tk", "tcl_tk")
  , ("TensorFlow", "tensorflow")
  , ("Twig", "sphinx")
  , ("TypeScript", "typescript")
  , ("Underscore.js", "underscore")
  , ("Vagrant", "vagrant")
  , ("Vue.js", "vue")
  , ("Vulkan", "vulkan")
  , ("webpack", "webpack")
  , ("XSLT & XPath", "mdn")
  , ("Yarn", "yarn")
  , ("Yii", "yii")
  ]
