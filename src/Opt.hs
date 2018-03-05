{-# LANGUAGE OverloadedStrings #-}

module Opt
  ( T(..)
  , get
  , Ground(..)
  ) where

import Options.Applicative
import Data.Monoid

data Ground = Foreground | Background

data T
  = StartGUI Ground
  | InstallDevdocs [String]


optParser :: ParserInfo T
optParser =
  info
    (optParser' <**> helper)
    (progDesc "A documentation browser for developers")
  where
    optParser' :: Parser T
    optParser' =
      startGUIParser <|> installDevdocsParser <|> pure (StartGUI Background)

startGUIParser :: Parser T
startGUIParser =
  flag' StartGUI
    (  long "gui"
    <> help "Start the GUI. This is the default behaviour")
  <*> flag Background Foreground
    (  long "foreground"
    <> help "Run the GUI in foreground")


installDevdocsParser :: Parser T
installDevdocsParser =
  InstallDevdocs . words <$>
    strOption
      (  long "install-devdocs"
      <> metavar "DOC1 DOC2 ..."
      <> help "Install DevDocs' docset. Separate multiple docsets with a space")

get :: IO T
get = execParser optParser
