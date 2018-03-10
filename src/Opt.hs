{-# LANGUAGE OverloadedStrings #-}

module Opt
  ( T(..)
  , get
  , Ground(..)
  ) where

import Options.Applicative
import Data.Monoid

data Ground
  = Foreground
  | Background
  deriving (Show)

data T
  = StartGUI Ground
  | InstallDevDocs [String]
  | InstallHoogle String String
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
        <|> installHoogleParser
        <|> pure (StartGUI Background)

startGUIParser :: Parser T
startGUIParser =
  flag' StartGUI
    (  long "gui"
    <> help "Start the GUI. This is the default behaviour")
  <*> flag Background Foreground
    (  long "foreground"
    <> help "Run the GUI in foreground")


installDevDocsParser :: Parser T
installDevDocsParser =
  flag' InstallDevDocs
    (  long "install-devdocs"
    <> help "Install DevDocs' docset")
  <*> some (strArgument
    (  metavar "DOC"
    <> help "Docset to install, like \"haskell\", \"python\""
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
  <*> strArgument
    (  metavar "NAME"
    <> help "Name of the database and documentation directory. Something like \"lts-10.8\" would be a good choice")

get :: IO T
get = execParser optParser
