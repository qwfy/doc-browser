{-# LANGUAGE OverloadedStrings #-}

module Opt
  ( T(..)
  , get
  ) where

import Options.Applicative
import Data.Semigroup

data T
  = StartGUI
  | InstallDevdocs [String]


optParser :: ParserInfo T
optParser =
  info
    (optParser' <**> helper)
    (progDesc "A documentation browser for developers")
  where
    optParser' :: Parser T
    optParser' =
      startGUIParser <|> installDevdocsParser <|> pure StartGUI


startGUIParser :: Parser T
startGUIParser =
  flag' StartGUI
    (  long "gui"
    <> help "Start the GUI. This is the default behaviour"
    )


installDevdocsParser :: Parser T
installDevdocsParser = do
  (InstallDevdocs . words) <$>
    (strOption
      (  long "install-devdocs"
      <> metavar "DOC1 DOC2 ..."
      <> help "Install devdocs' doc set. Separate multiple doc sets with a space"
      )
    )

get :: IO T
get = execParser optParser
