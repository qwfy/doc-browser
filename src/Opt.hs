{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opt
  ( T(..)
  , get
  , Logging(..)
  ) where

import Options.Applicative
import Data.Monoid

import qualified Doc

data T
  = StartGUI Logging
  | InstallDevDocs [Doc.Collection]
  | ListInstalledDevDocs
  | InstallHoogle String Doc.Collection
  | PrintPublicAPI
  | PrintDefaultConfig
  | PrintPort
  deriving (Show)

data Logging
  = NoLog
  | Log
  deriving(Show)

data IsStartedOK
  = StartedOK
  | StartedNotOK
  deriving (Show)


optParser :: ParserInfo T
optParser =
  info
    (optParser' <**> helper)
    (progDesc "A documentation browser for developers")
  where
    optParser' :: Parser T
    optParser' =
      startGUIParser
        <|> installDevDocsParser
        <|> listInstalledDevDocsParser
        <|> installHoogleParser
        <|> printPublicAPIParser
        <|> printDefaultConfigParser
        <|> printPortParser
        <|> pure (StartGUI NoLog)

startGUIParser :: Parser T
startGUIParser =
  flag' StartGUI
    (  long "gui"
    <> help "Start the GUI. This is the default behaviour")
  <*> flag NoLog Log
    (  long "debug"
    <> help "Write some debug information to stdout, I'm sorry if you need this")

readCollection :: ReadM Doc.Collection
readCollection = eitherReader $ \str' ->
  case Doc.parseCollection str' of
    Left e -> Left $ show e
    Right x -> Right x

installDevDocsParser :: Parser T
installDevDocsParser =
  flag' InstallDevDocs
    (  long "install-devdocs"
    <> help "Install DevDocs' docset")
  <*> some (argument readCollection
    (  metavar "DOC"
    <> help "Docset to install, like \"haskell\", \"python\""
    ))

listInstalledDevDocsParser :: Parser T
listInstalledDevDocsParser =
  flag' ListInstalledDevDocs
    (  long "list-installed-devdocs"
    <> help "List installed DevDocs' docset")

installHoogleParser :: Parser T
installHoogleParser =
  flag' InstallHoogle
    (  long "install-hoogle"
    <> help "Generate a Hoogle database from an archive, so it can be queried later")
  <*> strArgument
    (  metavar "URL"
    <> help (unwords
        [ "The archive to read."
        , "It can either be a local file or a HTTP link,"
        , "but should be in the format of \".tar.xz\"."
        , "It expects the unpacked archive can be consumed by `hoogle generate --local=<unpack_dir>`"
        , "Example: https://s3.amazonaws.com/haddock.stackage.org/lts-10.8/bundle.tar.xz"
        ]))
  <*> (argument readCollection
    (  metavar "COLLECTION"
    <> help "Name of the database and documentation directory. Something like \"lts-10.8\" would be a good choice"))

printPublicAPIParser :: Parser T
printPublicAPIParser =
  flag' PrintPublicAPI
    (  long "print-api"
    <> help "Print the HTTP API")

printDefaultConfigParser :: Parser T
printDefaultConfigParser =
  flag' PrintDefaultConfig
    (  long "print-default-config"
    <> help "Print the default configuration, which has detailed documentation")

printPortParser :: Parser T
printPortParser =
  flag' PrintPort
    (  long "get-port"
    <> help "Find out which port does this application use")

get :: IO T
get = execParser optParser
