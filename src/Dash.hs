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
  , listRemote
  , b64EncodeCV
  , extraDirs3
  , allCollections
  , removeDashEntryPrefixFromPath
  ) where

import Control.Monad
import Control.Monad.Catch

import Path
import Path.IO

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.Extra
import Data.Maybe
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as Char8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Database.Persist.Sqlite
import Fmt

import Text.XML
import Text.XML.Lens hiding ((|>))
import Text.Regex.PCRE
import qualified Data.Array as Array

import qualified Doc
import qualified Opt
import Db
import Utils


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

installMany :: ConfigRoot -> [Doc.Collection] -> Opt.DownloadMethod -> IO ()
installMany configRoot collections downloadMethod = do
  putStrLn "=== Docsets are provided by https://kapeli.com/dash ==="
  report ["downloading", show $ length collections, "docsets"]
  forM_ collections (\coll -> installOne configRoot coll downloadMethod)

installOne :: ConfigRoot -> Doc.Collection -> Opt.DownloadMethod -> IO ()
installOne configRoot collection downloadMethod = do
  report ["fetching information about", show collection]
  Location{locVersion=version, locUrl} <- getLocation collection
  docHome <- getDocHome configRoot collection version
  withTempDir (getConfigRoot configRoot) "temp-dash-" $ \tempDir -> do
    archivePath <-
      case downloadMethod of
        Opt.UseBuiltinDownloader -> do
          let tempPath = tempDir </> [relfile|archive|]
          report ["downloading", locUrl, "to", toFilePath tempPath]
          downloadFile locUrl tempPath
          return tempPath
        Opt.DownloadManually -> do
          report [ "please download"
                 , locUrl
                 , "to your disk,"
                 , "and when the download is finished, input the absolute path of the downloaded file below and hit Enter:"
                 ]
          let getPath = do
                mdp <- getLine
                case parseAbsFile mdp of
                  Left e -> do
                    report ["Error:", show e]
                    getPath
                  Right p ->
                    return p
          getPath
    report ["unpacking to", toFilePath tempDir]
    systemUnpackTgzInto archivePath tempDir
    subdir <- parseRelDir $ show collection <..> "docset"
    renameDir (tempDir </> subdir) docHome
  let sourceDb = docHome </> extraDirs2 </> [relfile|docSet.dsidx|]
  insertOne configRoot collection version sourceDb
  report ["installed", show collection]

insertOne :: ConfigRoot -> Doc.Collection -> Doc.Version -> Path Abs File -> IO ()
insertOne configRoot collection version sourceDb = do
  sources <- runSqlite (toFilePath sourceDb |> Text.pack) . asSqlBackend $ do
    -- TODO @incomplete: the [Single Bool] type annotation is wrong, but I don't care
    _ <- rawSql "alter table searchIndex rename to search_index" [] :: DbMonad [Single Bool]
    rows <- selectList [] [] :: DbMonad [Entity SearchIndex]
    _ <- rawSql "alter table search_index rename to searchIndex" [] :: DbMonad [Single Bool]
    return rows

  let entries = sources
        |> map entityVal
        |> map (\si ->
            Entry
              { entryName = searchIndexName si
              , entryPath = searchIndexPath si
              , entryVendor = Doc.Dash
              , entryCollection = collection
              , entryVersion = version
              })

  let insertAll = insertMany_ entries :: DbMonad ()
  runSqlite (dbPathText configRoot) insertAll

listRemote :: IO ()
listRemote =
  let cs = Map.keys allCollections
  in putStr . fmt $ blockListF (map show cs)


-- turn: <dash_entry_menuDescription=tutorial/stats><dash_entry_name=Statistics%20(scipy.stats)>doc/tutorial/stats.html
-- into: doc/tutorial/stats.html
removeDashEntryPrefixFromPath :: String -> String
removeDashEntryPrefixFromPath old =
  let p = "^(<dash_entry_[^>]+>)+" :: String
      regex = makeRegex p :: Regex
  in case Array.elems <$> matchOnce regex old of
       Nothing -> old
       Just ((_offset@0, length):_) | length > 0 -> drop length old
       Just _ -> old


-- https://github.com/Kapeli/Dash-iOS.git/Dash/DHDocsetDownloader.m
allCollections :: Map Doc.Collection Text
allCollections = Map.fromList
  [ ([Doc.collection|NET_Framework|]         , "net"           )
  , ([Doc.collection|ActionScript|]          , "actionscript"  )
  , ([Doc.collection|Akka|]                  , "akka"          )
  , ([Doc.collection|Android|]               , "android"       )
  , ([Doc.collection|Angular|]               , "angular"       )
  , ([Doc.collection|AngularJS|]             , "angularjs"     )
  , ([Doc.collection|Ansible|]               , "ansible"       )
  , ([Doc.collection|Apache_HTTP_Server|]    , "apache"        )
  , ([Doc.collection|Appcelerator_Titanium|] , "titanium"      )
  , ([Doc.collection|Apple_API_Reference|]   , "apple"         )
  , ([Doc.collection|AppleScript|]           , "applescript"   )
  , ([Doc.collection|Arduino|]               , "arduino"       )
  , ([Doc.collection|AWS_JavaScript|]        , "awsjs"         )
  , ([Doc.collection|BackboneJS|]            , "backbone"      )
  , ([Doc.collection|Bash|]                  , "bash"          )
  , ([Doc.collection|Boost|]                 , "boost"         )
  , ([Doc.collection|Bootstrap_2|]           , "bootstrap"     )
  , ([Doc.collection|Bootstrap_3|]           , "bootstrap"     )
  , ([Doc.collection|Bootstrap_4|]           , "bootstrap"     )
  , ([Doc.collection|Bourbon|]               , "bourbon"       )
  , ([Doc.collection|C|]                     , "c"             )
  , ([Doc.collection|C++|]                   , "cpp"           )
  , ([Doc.collection|CakePHP|]               , "cakephp"       )
  , ([Doc.collection|Cappuccino|]            , "cappuccino"    )
  , ([Doc.collection|Chai|]                  , "chai"          )
  , ([Doc.collection|Chef|]                  , "chef"          )
  , ([Doc.collection|Clojure|]               , "clojure"       )
  , ([Doc.collection|CMake|]                 , "cmake"         )
  , ([Doc.collection|Cocos2D|]               , "cocos2d"       )
  , ([Doc.collection|Cocos2D-X|]             , "cocos2dx"      )
  , ([Doc.collection|Cocos3D|]               , "cocos2d"       )
  , ([Doc.collection|CodeIgniter|]           , "codeigniter"   )
  , ([Doc.collection|CoffeeScript|]          , "coffee"        )
  , ([Doc.collection|ColdFusion|]            , "cf"            )
  , ([Doc.collection|Common_Lisp|]           , "lisp"          )
  , ([Doc.collection|Compass|]               , "compass"       )
  , ([Doc.collection|Cordova|]               , "cordova"       )
  , ([Doc.collection|Corona|]                , "corona"        )
  , ([Doc.collection|CouchDB|]               , "couchdb"       )
  , ([Doc.collection|Craft|]                 , "craft"         )
  , ([Doc.collection|CSS|]                   , "css"           )
  , ([Doc.collection|D3JS|]                  , "d3"            )
  , ([Doc.collection|Dart|]                  , "dartlang"      )
  , ([Doc.collection|Django|]                , "django"        )
  , ([Doc.collection|Doctrine_ORM|]          , "doctrine"      )
  , ([Doc.collection|Docker|]                , "docker"        )
  , ([Doc.collection|Dojo|]                  , "dojo"          )
  , ([Doc.collection|DOM|]                   , "dom"           )
  , ([Doc.collection|Drupal_7|]              , "drupal"        )
  , ([Doc.collection|Drupal_8|]              , "drupal"        )
  , ([Doc.collection|ElasticSearch|]         , "elasticsearch" )
  , ([Doc.collection|Elixir|]                , "elixir"        )
  , ([Doc.collection|Emacs_Lisp|]            , "elisp"         )
  , ([Doc.collection|EmberJS|]               , "ember"         )
  , ([Doc.collection|Emmet|]                 , "emmet"         )
  , ([Doc.collection|Erlang|]                , "erlang"        )
  , ([Doc.collection|Express|]               , "express"       )
  , ([Doc.collection|ExpressionEngine|]      , "ee"            )
  , ([Doc.collection|ExtJS|]                 , "extjs"         )
  , ([Doc.collection|Flask|]                 , "flask"         )
  , ([Doc.collection|Font_Awesome|]          , "awesome"       )
  , ([Doc.collection|Foundation|]            , "foundation"    )
  , ([Doc.collection|GLib|]                  , "glib"          )
  , ([Doc.collection|Go|]                    , "go"            )
  , ([Doc.collection|Gradle_DSL|]            , "gradle"        )
  , ([Doc.collection|Gradle_Java_API|]       , "gradle"        )
  , ([Doc.collection|Gradle_User_Guide|]     , "gradle"        )
  , ([Doc.collection|Grails|]                , "grails"        )
  , ([Doc.collection|Groovy|]                , "groovy"        )
  , ([Doc.collection|Groovy_JDK|]            , "groovy"        )
  , ([Doc.collection|Grunt|]                 , "grunt"         )
  , ([Doc.collection|Gulp|]                  , "gulp"          )
  , ([Doc.collection|Haml|]                  , "haml"          )
  , ([Doc.collection|Handlebars|]            , "handlebars"    )
  , ([Doc.collection|Haskell|]               , "haskell"       )
  , ([Doc.collection|HTML|]                  , "html"          )
  , ([Doc.collection|Ionic|]                 , "ionic"         )
  , ([Doc.collection|iOS|]                   , "iphone"        )
  , ([Doc.collection|Jasmine|]               , "jasmine"       )
  , ([Doc.collection|Java_EE6|]              , "jee6"          )
  , ([Doc.collection|Java_EE7|]              , "jee7"          )
  , ([Doc.collection|Java_EE8|]              , "jee8"          )
  , ([Doc.collection|Java_SE6|]              , "java"          )
  , ([Doc.collection|Java_SE7|]              , "java"          )
  , ([Doc.collection|Java_SE8|]              , "java"          )
  , ([Doc.collection|Java_SE9|]              , "java"          )
  , ([Doc.collection|Java_SE10|]             , "java"          )
  , ([Doc.collection|JavaScript|]            , "javascript"    )
  , ([Doc.collection|Jekyll|]                , "jekyll"        )
  , ([Doc.collection|Jinja|]                 , "jinja"         )
  , ([Doc.collection|Joomla|]                , "joomla"        )
  , ([Doc.collection|jQuery|]                , "jQuery"        )
  , ([Doc.collection|jQuery_Mobile|]         , "jquerym"       )
  , ([Doc.collection|jQuery_UI|]             , "jqueryui"      )
  , ([Doc.collection|Julia|]                 , "julia"         )
  , ([Doc.collection|KnockoutJS|]            , "knockout"      )
  , ([Doc.collection|Kobold2D|]              , "kobold2d"      )
  , ([Doc.collection|LaTeX|]                 , "latex"         )
  , ([Doc.collection|Laravel|]               , "laravel"       )
  , ([Doc.collection|Less|]                  , "less"          )
  , ([Doc.collection|Lo-Dash|]               , "lodash"        )
  , ([Doc.collection|Lua_5.1|]               , "lua"           )
  , ([Doc.collection|Lua_5.2|]               , "lua"           )
  , ([Doc.collection|Lua_5.3|]               , "lua"           )
  , ([Doc.collection|MarionetteJS|]          , "marionette"    )
  , ([Doc.collection|Markdown|]              , "markdown"      )
  , ([Doc.collection|MatPlotLib|]            , "matplotlib"    )
  , ([Doc.collection|Meteor|]                , "meteor"        )
  , ([Doc.collection|Mocha|]                 , "mocha"         )
  , ([Doc.collection|MomentJS|]              , "moment"        )
  , ([Doc.collection|MongoDB|]               , "mongodb"       )
  , ([Doc.collection|Mongoose|]              , "mongoose"      )
  , ([Doc.collection|Mono|]                  , "mono"          )
  , ([Doc.collection|MooTools|]              , "moo"           )
  , ([Doc.collection|MySQL|]                 , "mysql"         )
  , ([Doc.collection|Neat|]                  , "neat"          )
  , ([Doc.collection|Nginx|]                 , "nginx"         )
  , ([Doc.collection|NodeJS|]                , "nodejs"        )
  , ([Doc.collection|NumPy|]                 , "numpy"         )
  , ([Doc.collection|OCaml|]                 , "ocaml"         )
  , ([Doc.collection|OpenCV|]                , "opencv"        )
  , ([Doc.collection|OpenGL_2|]              , "gl2"           )
  , ([Doc.collection|OpenGL_3|]              , "gl3"           )
  , ([Doc.collection|OpenGL_4|]              , "gl4"           )
  , ([Doc.collection|macOS|]                 , "Mac"           )
  , ([Doc.collection|Pandas|]                , "pandas"        )
  , ([Doc.collection|Perl|]                  , "perl"          )
  , ([Doc.collection|Phalcon|]               , "phalcon"       )
  , ([Doc.collection|PhoneGap|]              , "phonegap"      )
  , ([Doc.collection|PHP|]                   , "php"           )
  , ([Doc.collection|PHPUnit|]               , "phpunit"       )
  , ([Doc.collection|Play_Java|]             , "playjava"      )
  , ([Doc.collection|Play_Scala|]            , "playscala"     )
  , ([Doc.collection|Polymer.dart|]          , "polymerdart"   )
  , ([Doc.collection|PostgreSQL|]            , "psql"          )
  , ([Doc.collection|Processing|]            , "processing"    )
  , ([Doc.collection|PrototypeJS|]           , "prototype"     )
  , ([Doc.collection|Pug|]                   , "pug"           )
  , ([Doc.collection|Puppet|]                , "puppet"        )
  , ([Doc.collection|Python_2|]              , "python"        )
  , ([Doc.collection|Python_3|]              , "python"        )
  , ([Doc.collection|Qt_4|]                  , "qt"            )
  , ([Doc.collection|Qt_5|]                  , "qt"            )
  , ([Doc.collection|R|]                     , "r"             )
  , ([Doc.collection|Racket|]                , "racket"        )
  , ([Doc.collection|React|]                 , "react"         )
  , ([Doc.collection|Redis|]                 , "redis"         )
  , ([Doc.collection|RequireJS|]             , "require"       )
  , ([Doc.collection|Ruby|]                  , "ruby"          )
  , ([Doc.collection|Ruby_2|]                , "ruby"          )
  , ([Doc.collection|Ruby_on_Rails_3|]       , "rails"         )
  , ([Doc.collection|Ruby_on_Rails_4|]       , "rails"         )
  , ([Doc.collection|Ruby_on_Rails_5|]       , "rails"         )
  , ([Doc.collection|RubyMotion|]            , "rubymotion"    )
  , ([Doc.collection|Rust|]                  , "rust"          )
  , ([Doc.collection|SailsJS|]               , "sails"         )
  , ([Doc.collection|SaltStack|]             , "salt"          )
  , ([Doc.collection|Sass|]                  , "sass"          )
  , ([Doc.collection|Scala|]                 , "scala"         )
  , ([Doc.collection|SciPy|]                 , "scipy"         )
  , ([Doc.collection|Semantic_UI|]           , "semantic"      )
  , ([Doc.collection|Sencha_Touch|]          , "sencha"        )
  , ([Doc.collection|Sinon|]                 , "sinon"         )
  , ([Doc.collection|Smarty|]                , "smarty"        )
  , ([Doc.collection|Sparrow|]               , "sparrow"       )
  , ([Doc.collection|Spring_Framework|]      , "spring"        )
  , ([Doc.collection|SproutCore|]            , "SproutCore"    )
  , ([Doc.collection|SQLAlchemy|]            , "sqlalchemy"    )
  , ([Doc.collection|SQLite|]                , "sqlite"        )
  , ([Doc.collection|Statamic|]              , "statamic"      )
  , ([Doc.collection|Stylus|]                , "stylus"        )
  , ([Doc.collection|Susy|]                  , "susy"          )
  , ([Doc.collection|SVG|]                   , "svg"           )
  , ([Doc.collection|Swift|]                 , "swift"         )
  , ([Doc.collection|Symfony|]               , "symfony"       )
  , ([Doc.collection|Tcl|]                   , "tcl"           )
  , ([Doc.collection|Tornado|]               , "tornado"       )
  , ([Doc.collection|tvOS|]                  , "tvos"          )
  , ([Doc.collection|Twig|]                  , "twig"          )
  , ([Doc.collection|Twisted|]               , "twisted"       )
  , ([Doc.collection|TypeScript|]            , "typescript"    )
  , ([Doc.collection|TYPO3|]                 , "typo3"         )
  , ([Doc.collection|UnderscoreJS|]          , "underscore"    )
  , ([Doc.collection|Unity_3D|]              , "unity3d"       )
  , ([Doc.collection|Vagrant|]               , "vagrant"       )
  , ([Doc.collection|Vim|]                   , "vim"           )
  , ([Doc.collection|VMware_vSphere|]        , "vsphere"       )
  , ([Doc.collection|VueJS|]                 , "vue"           )
  , ([Doc.collection|watchOS|]               , "watchos"       )
  , ([Doc.collection|WordPress|]             , "wordpress"     )
  , ([Doc.collection|Xamarin|]               , "xamarin"       )
  , ([Doc.collection|Xojo|]                  , "xojo"          )
  , ([Doc.collection|XSLT|]                  , "xslt"          )
  , ([Doc.collection|Yii|]                   , "yii"           )
  , ([Doc.collection|YUI|]                   , "yui"           )
  , ([Doc.collection|Zend_Framework_1|]      , "zend"          )
  , ([Doc.collection|Zend_Framework_2|]      , "zend"          )
  , ([Doc.collection|Zend_Framework_3|]      , "zend"          )
  , ([Doc.collection|ZeptoJS|]               , "zepto"         )
  ]
