{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dash
  ( installMany
  , b64EncodeCV
  , extraDirs3
  ) where

import GHC.Generics (Generic)

import qualified Network.URI as URI
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Path
import Path.IO

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.List.Extra
import Data.Maybe
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map

import Database.Persist.Sqlite
import Database.Persist.TH
import Fmt

import Text.XML
import Text.XML.Lens hiding ((|>))

import qualified Doc
import qualified Db
import Utils


share [mkPersist sqlSettings] [persistLowerCase|
SearchIndex
  name String
  type String
  path String
  deriving Show
|]

data Location = Location
  { locVersion :: Doc.Version
  , locUrl :: String
  }

feedsUrl = "https://kapeli.com/feeds"

extraDirs2 = [reldir|Contents/Resources|]
extraDirs3 = [reldir|Contents/Resources/Documents|]

getLocation :: Doc.Collection -> IO Location
getLocation coll = do
  let url = feedsUrl <//> Doc.getCollection coll <..> "xml"
  bs <- download url
  let doc = parseLBS_ def bs
  return $ Location
    { locVersion = Doc.Version . trim . Text.unpack . fromJust $ (doc ^? root ./ el "version" . text)
    , locUrl = trim. Text.unpack . fromJust $ (doc ^? root ./ el "url" . text)
    }

b64EncodeCV :: Doc.Collection -> Doc.Version -> String
b64EncodeCV collection version = do
  -- TODO @incomplete: unify with DevDocs
  Char8.unpack . B64.encode . Char8.pack $ show collection ++ "==" ++ show version

getDocHome :: MonadThrow m => ConfigRoot -> Doc.Collection -> Doc.Version -> m (Path Abs Dir)
getDocHome configRoot collection version = do
  vendorPart <- parseRelDir $ show Doc.Dash
  -- TODO @incomplete: unify with DevDocs
  collPart <- parseRelDir $ b64EncodeCV collection version
  return $ getConfigRoot configRoot </> vendorPart </> collPart

installMany :: ConfigRoot -> [Doc.Collection] -> IO ()
installMany configRoot collections = do
  putStrLn "=== Docsets are provided by https://kapeli.com/dash ==="
  report ["downloading", show $ length collections, "docsets"]
  forM_ collections (installOne configRoot)

installOne :: ConfigRoot -> Doc.Collection -> IO ()
installOne configRoot collection = do
  report ["fetching information about", show collection]
  Location{locVersion=version, locUrl} <- getLocation collection
  docHome <- getDocHome configRoot collection version
  withTempDir (getConfigRoot configRoot) "temp-dash-" $ \tempDir -> do
    let archivePath = tempDir </> [relfile|archive|]
    report ["downloading", locUrl, "to", toFilePath archivePath]
    downloadFile locUrl archivePath
    report ["unpacking to", toFilePath tempDir]
    systemUnpackTgzInto archivePath tempDir
    subdir <- parseRelDir $ show collection <..> "docset"
    renameDir (tempDir </> subdir) docHome
  let sourceDb = docHome </> extraDirs2 </> [relfile|docSet.dsidx|]
  insertOne configRoot collection version sourceDb
  report ["installed", show collection]

insertOne :: ConfigRoot -> Doc.Collection -> Doc.Version -> Path Abs File -> IO ()
insertOne configRoot collection version sourceDb = do
  sources <- runSqlite (toFilePath sourceDb |> Text.pack) . Db.asSqlBackend $ do
    -- TODO @incomplete: the [Single Bool] type annotation is wrong, but I don't care
    _ <- rawSql "alter table searchIndex rename to search_index" [] :: Db.DbMonad [Single Bool]
    rows <- selectList [] [] :: Db.DbMonad [Entity SearchIndex]
    _ <- rawSql "alter table search_index rename to searchIndex" [] :: Db.DbMonad [Single Bool]
    return rows

  let entries = sources
        |> map entityVal
        |> map (\si ->
            Db.Entry
              { Db.entryName = searchIndexName si
              , Db.entryPath = searchIndexPath si
              , Db.entryVendor = Doc.Dash
              , Db.entryCollection = collection
              , Db.entryVersion = version
              })

  let insertAll = insertMany_ entries :: Db.DbMonad ()
  runSqlite (Db.dbPathText configRoot) insertAll

data HasVersion = HasVersion | HasNoVersion

-- https://github.com/Kapeli/Dash-iOS.git/Dash/DHDocsetDownloader.m
index = Map.fromList
  [ ([Doc.collection|NET_Framework|]         , ("net"            , HasVersion))
  , ([Doc.collection|ActionScript|]          , ("actionscript"   , HasNoVersion))
  , ([Doc.collection|Akka|]                  , ("akka"           , HasVersion))
  , ([Doc.collection|Android|]               , ("android"        , HasVersion))
  , ([Doc.collection|Angular|]               , ("angular"        , HasVersion))
  , ([Doc.collection|AngularJS|]             , ("angularjs"      , HasVersion))
  , ([Doc.collection|Ansible|]               , ("ansible"        , HasVersion))
  , ([Doc.collection|Apache_HTTP_Server|]    , ("apache"         , HasVersion))
  , ([Doc.collection|Appcelerator_Titanium|] , ("titanium"       , HasVersion))
  , ([Doc.collection|Apple_API_Reference|]   , ("apple"          , HasNoVersion))
  , ([Doc.collection|AppleScript|]           , ("applescript"    , HasNoVersion))
  , ([Doc.collection|Arduino|]               , ("arduino"        , HasVersion))
  , ([Doc.collection|AWS_JavaScript|]        , ("awsjs"          , HasVersion))
  , ([Doc.collection|BackboneJS|]            , ("backbone"       , HasVersion))
  , ([Doc.collection|Bash|]                  , ("bash"           , HasNoVersion))
  , ([Doc.collection|Boost|]                 , ("boost"          , HasVersion))
  , ([Doc.collection|Bootstrap_2|]           , ("bootstrap"      , HasVersion))
  , ([Doc.collection|Bootstrap_3|]           , ("bootstrap"      , HasVersion))
  , ([Doc.collection|Bootstrap_4|]           , ("bootstrap"      , HasVersion))
  , ([Doc.collection|Bourbon|]               , ("bourbon"        , HasVersion))
  , ([Doc.collection|C|]                     , ("c"              , HasNoVersion))
  , ([Doc.collection|C++|]                   , ("cpp"            , HasNoVersion))
  , ([Doc.collection|CakePHP|]               , ("cakephp"        , HasVersion))
  , ([Doc.collection|Cappuccino|]            , ("cappuccino"     , HasVersion))
  , ([Doc.collection|Chai|]                  , ("chai"           , HasVersion))
  , ([Doc.collection|Chef|]                  , ("chef"           , HasVersion))
  , ([Doc.collection|Clojure|]               , ("clojure"        , HasVersion))
  , ([Doc.collection|CMake|]                 , ("cmake"          , HasVersion))
  , ([Doc.collection|Cocos2D|]               , ("cocos2d"        , HasVersion))
  , ([Doc.collection|Cocos2D-X|]             , ("cocos2dx"       , HasVersion))
  , ([Doc.collection|Cocos3D|]               , ("cocos2d"        , HasVersion))
  , ([Doc.collection|CodeIgniter|]           , ("codeigniter"    , HasVersion))
  , ([Doc.collection|CoffeeScript|]          , ("coffee"         , HasVersion))
  , ([Doc.collection|ColdFusion|]            , ("cf"             , HasVersion))
  , ([Doc.collection|Common_Lisp|]           , ("lisp"           , HasNoVersion))
  , ([Doc.collection|Compass|]               , ("compass"        , HasVersion))
  , ([Doc.collection|Cordova|]               , ("cordova"        , HasVersion))
  , ([Doc.collection|Corona|]                , ("corona"         , HasVersion))
  , ([Doc.collection|CouchDB|]               , ("couchdb"        , HasVersion))
  , ([Doc.collection|Craft|]                 , ("craft"          , HasVersion))
  , ([Doc.collection|CSS|]                   , ("css"            , HasNoVersion))
  , ([Doc.collection|D3JS|]                  , ("d3"             , HasVersion))
  , ([Doc.collection|Dart|]                  , ("dartlang"       , HasVersion))
  , ([Doc.collection|Django|]                , ("django"         , HasVersion))
  , ([Doc.collection|Doctrine_ORM|]          , ("doctrine"       , HasVersion))
  , ([Doc.collection|Docker|]                , ("docker"         , HasVersion))
  , ([Doc.collection|Dojo|]                  , ("dojo"           , HasVersion))
  , ([Doc.collection|DOM|]                   , ("dom"            , HasNoVersion))
  , ([Doc.collection|Drupal_7|]              , ("drupal"         , HasVersion))
  , ([Doc.collection|Drupal_8|]              , ("drupal"         , HasVersion))
  , ([Doc.collection|ElasticSearch|]         , ("elasticsearch"  , HasVersion))
  , ([Doc.collection|Elixir|]                , ("elixir"         , HasVersion))
  , ([Doc.collection|Emacs_Lisp|]            , ("elisp"          , HasNoVersion))
  , ([Doc.collection|EmberJS|]               , ("ember"          , HasVersion))
  , ([Doc.collection|Emmet|]                 , ("emmet"          , HasNoVersion))
  , ([Doc.collection|Erlang|]                , ("erlang"         , HasVersion))
  , ([Doc.collection|Express|]               , ("express"        , HasVersion))
  , ([Doc.collection|ExpressionEngine|]      , ("ee"             , HasVersion))
  , ([Doc.collection|ExtJS|]                 , ("extjs"          , HasVersion))
  , ([Doc.collection|Flask|]                 , ("flask"          , HasVersion))
  , ([Doc.collection|Font_Awesome|]          , ("awesome"        , HasVersion))
  , ([Doc.collection|Foundation|]            , ("foundation"     , HasVersion))
  , ([Doc.collection|GLib|]                  , ("glib"           , HasVersion))
  , ([Doc.collection|Go|]                    , ("go"             , HasVersion))
  , ([Doc.collection|Gradle_DSL|]            , ("gradle"         , HasVersion))
  , ([Doc.collection|Gradle_Java_API|]       , ("gradle"         , HasVersion))
  , ([Doc.collection|Gradle_User_Guide|]     , ("gradle"         , HasVersion))
  , ([Doc.collection|Grails|]                , ("grails"         , HasVersion))
  , ([Doc.collection|Groovy|]                , ("groovy"         , HasVersion))
  , ([Doc.collection|Groovy_JDK|]            , ("groovy"         , HasVersion))
  , ([Doc.collection|Grunt|]                 , ("grunt"          , HasVersion))
  , ([Doc.collection|Gulp|]                  , ("gulp"           , HasVersion))
  , ([Doc.collection|Haml|]                  , ("haml"           , HasVersion))
  , ([Doc.collection|Handlebars|]            , ("handlebars"     , HasVersion))
  , ([Doc.collection|Haskell|]               , ("haskell"        , HasVersion))
  , ([Doc.collection|HTML|]                  , ("html"           , HasNoVersion))
  , ([Doc.collection|Ionic|]                 , ("ionic"          , HasVersion))
  , ([Doc.collection|iOS|]                   , ("iphone"         , HasVersion))
  , ([Doc.collection|Jasmine|]               , ("jasmine"        , HasVersion))
  , ([Doc.collection|Java_EE6|]              , ("jee6"           , HasNoVersion))
  , ([Doc.collection|Java_EE7|]              , ("jee7"           , HasNoVersion))
  , ([Doc.collection|Java_EE8|]              , ("jee8"           , HasNoVersion))
  , ([Doc.collection|Java_SE6|]              , ("java"           , HasNoVersion))
  , ([Doc.collection|Java_SE7|]              , ("java"           , HasNoVersion))
  , ([Doc.collection|Java_SE8|]              , ("java"           , HasNoVersion))
  , ([Doc.collection|Java_SE9|]              , ("java"           , HasNoVersion))
  , ([Doc.collection|Java_SE10|]             , ("java"           , HasNoVersion))
  , ([Doc.collection|JavaScript|]            , ("javascript"     , HasNoVersion))
  , ([Doc.collection|Jekyll|]                , ("jekyll"         , HasVersion))
  , ([Doc.collection|Jinja|]                 , ("jinja"          , HasVersion))
  , ([Doc.collection|Joomla|]                , ("joomla"         , HasVersion))
  , ([Doc.collection|jQuery|]                , ("jQuery"         , HasVersion))
  , ([Doc.collection|jQuery_Mobile|]         , ("jquerym"        , HasVersion))
  , ([Doc.collection|jQuery_UI|]             , ("jqueryui"       , HasVersion))
  , ([Doc.collection|Julia|]                 , ("julia"          , HasVersion))
  , ([Doc.collection|KnockoutJS|]            , ("knockout"       , HasVersion))
  , ([Doc.collection|Kobold2D|]              , ("kobold2d"       , HasVersion))
  , ([Doc.collection|LaTeX|]                 , ("latex"          , HasNoVersion))
  , ([Doc.collection|Laravel|]               , ("laravel"        , HasVersion))
  , ([Doc.collection|Less|]                  , ("less"           , HasVersion))
  , ([Doc.collection|Lo-Dash|]               , ("lodash"         , HasVersion))
  , ([Doc.collection|Lua_5.1|]               , ("lua"            , HasNoVersion))
  , ([Doc.collection|Lua_5.2|]               , ("lua"            , HasNoVersion))
  , ([Doc.collection|Lua_5.3|]               , ("lua"            , HasNoVersion))
  , ([Doc.collection|MarionetteJS|]          , ("marionette"     , HasVersion))
  , ([Doc.collection|Markdown|]              , ("markdown"       , HasNoVersion))
  , ([Doc.collection|MatPlotLib|]            , ("matplotlib"     , HasVersion))
  , ([Doc.collection|Meteor|]                , ("meteor"         , HasVersion))
  , ([Doc.collection|Mocha|]                 , ("mocha"          , HasVersion))
  , ([Doc.collection|MomentJS|]              , ("moment"         , HasVersion))
  , ([Doc.collection|MongoDB|]               , ("mongodb"        , HasVersion))
  , ([Doc.collection|Mongoose|]              , ("mongoose"       , HasVersion))
  , ([Doc.collection|Mono|]                  , ("mono"           , HasNoVersion))
  , ([Doc.collection|MooTools|]              , ("moo"            , HasVersion))
  , ([Doc.collection|MySQL|]                 , ("mysql"          , HasVersion))
  , ([Doc.collection|Neat|]                  , ("neat"           , HasVersion))
  , ([Doc.collection|Nginx|]                 , ("nginx"          , HasVersion))
  , ([Doc.collection|NodeJS|]                , ("nodejs"         , HasVersion))
  , ([Doc.collection|NumPy|]                 , ("numpy"          , HasVersion))
  , ([Doc.collection|OCaml|]                 , ("ocaml"          , HasVersion))
  , ([Doc.collection|OpenCV|]                , ("opencv"         , HasVersion))
  , ([Doc.collection|OpenGL_2|]              , ("gl2"            , HasNoVersion))
  , ([Doc.collection|OpenGL_3|]              , ("gl3"            , HasNoVersion))
  , ([Doc.collection|OpenGL_4|]              , ("gl4"            , HasNoVersion))
  , ([Doc.collection|macOS|]                 , ("Mac"            , HasVersion))
  , ([Doc.collection|Pandas|]                , ("pandas"         , HasVersion))
  , ([Doc.collection|Perl|]                  , ("perl"           , HasVersion))
  , ([Doc.collection|Phalcon|]               , ("phalcon"        , HasVersion))
  , ([Doc.collection|PhoneGap|]              , ("phonegap"       , HasVersion))
  , ([Doc.collection|PHP|]                   , ("php"            , HasNoVersion))
  , ([Doc.collection|PHPUnit|]               , ("phpunit"        , HasVersion))
  , ([Doc.collection|Play_Java|]             , ("playjava"       , HasVersion))
  , ([Doc.collection|Play_Scala|]            , ("playscala"      , HasVersion))
  , ([Doc.collection|Polymer.dart|]          , ("polymerdart"    , HasVersion))
  , ([Doc.collection|PostgreSQL|]            , ("psql"           , HasVersion))
  , ([Doc.collection|Processing|]            , ("processing"     , HasVersion))
  , ([Doc.collection|PrototypeJS|]           , ("prototype"      , HasVersion))
  , ([Doc.collection|Pug|]                   , ("pug"            , HasVersion))
  , ([Doc.collection|Puppet|]                , ("puppet"         , HasVersion))
  , ([Doc.collection|Python_2|]              , ("python"         , HasVersion))
  , ([Doc.collection|Python_3|]              , ("python"         , HasVersion))
  , ([Doc.collection|Qt_4|]                  , ("qt"             , HasVersion))
  , ([Doc.collection|Qt_5|]                  , ("qt"             , HasVersion))
  , ([Doc.collection|R|]                     , ("r"              , HasVersion))
  , ([Doc.collection|Racket|]                , ("racket"         , HasVersion))
  , ([Doc.collection|React|]                 , ("react"          , HasVersion))
  , ([Doc.collection|Redis|]                 , ("redis"          , HasVersion))
  , ([Doc.collection|RequireJS|]             , ("require"        , HasVersion))
  , ([Doc.collection|Ruby|]                  , ("ruby"           , HasVersion))
  , ([Doc.collection|Ruby_2|]                , ("ruby"           , HasVersion))
  , ([Doc.collection|Ruby_on_Rails_3|]       , ("rails"          , HasVersion))
  , ([Doc.collection|Ruby_on_Rails_4|]       , ("rails"          , HasVersion))
  , ([Doc.collection|Ruby_on_Rails_5|]       , ("rails"          , HasVersion))
  , ([Doc.collection|RubyMotion|]            , ("rubymotion"     , HasNoVersion))
  , ([Doc.collection|Rust|]                  , ("rust"           , HasVersion))
  , ([Doc.collection|SailsJS|]               , ("sails"          , HasVersion))
  , ([Doc.collection|SaltStack|]             , ("salt"           , HasVersion))
  , ([Doc.collection|Sass|]                  , ("sass"           , HasVersion))
  , ([Doc.collection|Scala|]                 , ("scala"          , HasVersion))
  , ([Doc.collection|SciPy|]                 , ("scipy"          , HasVersion))
  , ([Doc.collection|Semantic_UI|]           , ("semantic"       , HasVersion))
  , ([Doc.collection|Sencha_Touch|]          , ("sencha"         , HasVersion))
  , ([Doc.collection|Sinon|]                 , ("sinon"          , HasVersion))
  , ([Doc.collection|Smarty|]                , ("smarty"         , HasVersion))
  , ([Doc.collection|Sparrow|]               , ("sparrow"        , HasVersion))
  , ([Doc.collection|Spring_Framework|]      , ("spring"         , HasVersion))
  , ([Doc.collection|SproutCore|]            , ("SproutCore"     , HasVersion))
  , ([Doc.collection|SQLAlchemy|]            , ("sqlalchemy"     , HasVersion))
  , ([Doc.collection|SQLite|]                , ("sqlite"         , HasVersion))
  , ([Doc.collection|Statamic|]              , ("statamic"       , HasVersion))
  , ([Doc.collection|Stylus|]                , ("stylus"         , HasVersion))
  , ([Doc.collection|Susy|]                  , ("susy"           , HasVersion))
  , ([Doc.collection|SVG|]                   , ("svg"            , HasNoVersion))
  , ([Doc.collection|Swift|]                 , ("swift"          , HasNoVersion))
  , ([Doc.collection|Symfony|]               , ("symfony"        , HasVersion))
  , ([Doc.collection|Tcl|]                   , ("tcl"            , HasVersion))
  , ([Doc.collection|Tornado|]               , ("tornado"        , HasVersion))
  , ([Doc.collection|tvOS|]                  , ("tvos"           , HasNoVersion))
  , ([Doc.collection|Twig|]                  , ("twig"           , HasVersion))
  , ([Doc.collection|Twisted|]               , ("twisted"        , HasVersion))
  , ([Doc.collection|TypeScript|]            , ("typescript"     , HasVersion))
  , ([Doc.collection|TYPO3|]                 , ("typo3"          , HasVersion))
  , ([Doc.collection|UnderscoreJS|]          , ("underscore"     , HasVersion))
  , ([Doc.collection|Unity_3D|]              , ("unity3d"        , HasVersion))
  , ([Doc.collection|Vagrant|]               , ("vagrant"        , HasVersion))
  , ([Doc.collection|Vim|]                   , ("vim"            , HasVersion))
  , ([Doc.collection|VMware_vSphere|]        , ("vsphere"        , HasVersion))
  , ([Doc.collection|VueJS|]                 , ("vue"            , HasVersion))
  , ([Doc.collection|watchOS|]               , ("watchos"        , HasVersion))
  , ([Doc.collection|WordPress|]             , ("wordpress"      , HasNoVersion))
  , ([Doc.collection|Xamarin|]               , ("xamarin"        , HasNoVersion))
  , ([Doc.collection|Xojo|]                  , ("xojo"           , HasVersion))
  , ([Doc.collection|XSLT|]                  , ("xslt"           , HasNoVersion))
  , ([Doc.collection|Yii|]                   , ("yii"            , HasVersion))
  , ([Doc.collection|YUI|]                   , ("yui"            , HasVersion))
  , ([Doc.collection|Zend_Framework_1|]      , ("zend"           , HasVersion))
  , ([Doc.collection|Zend_Framework_2|]      , ("zend"           , HasVersion))
  , ([Doc.collection|Zend_Framework_3|]      , ("zend"           , HasVersion))
  , ([Doc.collection|ZeptoJS|]               , ("zepto"          , HasVersion))
  ]
