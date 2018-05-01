{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opt
  ( T(..)
  , get
  , Logging(..)
  ) where

import Options.Applicative
import Data.Monoid

import Path

import qualified Doc
import Utils

data T
  = StartGUI Logging

  | InstallDevDocs [Either Doc.Collection (Doc.Collection, Doc.Version)]
  | ListInstalledDevDocs
  | ListRemoteDevDocs
  | RemoveDevDocs [(Doc.Collection, Doc.Version)]

  | InstallDash [Doc.Collection]
  | ListInstalledDash
  | ListRemoteDash
  | RemoveDash [(Doc.Collection, Doc.Version)]

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
        <|> listRemoteDevDocsParser
        <|> removeDevDocsParser

        <|> installDashParser
        <|> listInstalledDashParser
        <|> listRemoteDashParser
        <|> removeDashParser

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

readCCV :: ReadM (Either Doc.Collection (Doc.Collection, Doc.Version))
readCCV = eitherReader $ \str ->
  if Doc.hasCVSep str
    then do
      cv <- mapLeft show $ parseRelDir str
      cvTuple <- mapLeft show $ Doc.breakCollectionVersion cv
      return (Right cvTuple)
    else do
      c <- mapLeft show $ Doc.parseCollection str
      return (Left c)

installDevDocsParser :: Parser T
installDevDocsParser =
  flag' InstallDevDocs
    (  long "install-devdocs"
    <> help "Install DevDocs docset")
  <*> some (argument readCCV
    (  metavar "DOC"
    <> (help . unwords $
        [ "Docset to install. It can either be fuzzy"
        , "(e.g. \"haskell\", \"python\"),"
        , "which will install the latest version of a matching docset,"
        , "or exact (e.g. \"Python==2.7.13\"),"
        , "which is a string in the format of COLLECTION==VERSION,"
        , "to install the exact docset."
        , "--list-remote-devdocs"
        , "will list all available docset in the exact format"
        ])))

listInstalledDevDocsParser :: Parser T
listInstalledDevDocsParser =
  flag' ListInstalledDevDocs
    (  long "list-installed-devdocs"
    <> help "List installed DevDocs docset")

listRemoteDevDocsParser :: Parser T
listRemoteDevDocsParser =
  flag' ListRemoteDevDocs
    (  long "list-remote-devdocs"
    <> help "List all available DevDocs docset")

removeDevDocsParser :: Parser T
removeDevDocsParser =
  flag' RemoveDevDocs
    (  long "remove-devdocs"
    <> help "Remove DevDocs docset")
  <*> some (argument readCollectionVersionTuple
    (  metavar "CV"
    <> help "A string in the format of COLLECTION==VERSION. Intended to be used with --list-installed-devdocs"
    ))

readCollectionVersionTuple :: ReadM (Doc.Collection, Doc.Version)
readCollectionVersionTuple = eitherReader $ \str -> do
  cvPath <- mapLeft show $ parseRelDir str
  mapLeft show (Doc.breakCollectionVersion cvPath)

readCollection :: ReadM Doc.Collection
readCollection = eitherReader $ \str' ->
  case Doc.parseCollection str' of
    Left e -> Left $ show e
    Right x -> Right x

installDashParser :: Parser T
installDashParser =
  flag' InstallDash
    (  long "install-dash"
    <> help "Install Dash docset")
  <*> some (argument readCollection
    (  metavar "COLLECTION"
    <> help "Collection to install. --list-remote-dash will list available collections"
    ))

listInstalledDashParser :: Parser T
listInstalledDashParser =
  flag' ListInstalledDash
    (  long "list-installed-dash"
    <> help "List installed Dash docset")

listRemoteDashParser :: Parser T
listRemoteDashParser =
  flag' ListRemoteDash
    (  long "list-remote-dash"
    <> help "List all available Dash docset")

removeDashParser :: Parser T
removeDashParser =
  flag' RemoveDash
    (  long "remove-dash"
    <> help "Remove Dash docset")
  <*> some (argument readCollectionVersionTuple
    (  metavar "CV"
    <> help "A string in the format of COLLECTION==VERSION. Intended to be used with --list-installed-dash"
    ))

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
