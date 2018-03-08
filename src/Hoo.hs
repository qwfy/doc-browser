{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findDatabase
  ) where

import qualified Safe
import Data.List.Extra
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Control.Monad

import qualified Hoogle

import qualified Match
import Utils

search :: Hoogle.Database -> Int -> String -> [Match.T]
search db limit query =
  Hoogle.searchDatabase db query
  |> take limit
  |> map toMatch

toMatch :: Hoogle.Target -> Match.T
toMatch target =
  let name' = unHTML . Hoogle.targetItem $ target
      (name, typeConstraint) = maybe
        (name', Nothing)
        (\(a, b) -> (a, Just b))
        (splitTypeConstraint name')
  in Match.T { Match.language = "Haskell"
             -- TODO @incomplete: don't hard code this
             , Match.version = "lts-10.8"
             , Match.name = Text.pack name
             -- TODO @incomplete: proper handling
             , Match.url = Text.pack $ Hoogle.targetURL target
             , Match.source = "hoogle"
             , Match.package_ = Text.pack . fst <$> Hoogle.targetPackage target
             , Match.module_ = Text.pack . fst <$> Hoogle.targetModule target
             , Match.typeConstraint = Text.pack <$> typeConstraint
             }


findDatabase :: FilePath -> IO (Maybe FilePath)
findDatabase configRoot = do
  let hoogleDir = joinPath [configRoot, "hoogle"]
  exist <- doesDirectoryExist hoogleDir
  if not exist
    then return Nothing
    else do
      paths <- listDirectory hoogleDir >>= filterM (doesFileExist . (hoogleDir </>))
      filter isRecognized paths
        |> sort
        -- currently, only load the latest
        |> Safe.lastMay
        |> fmap ("hoogle" </>)
        |> return
  where
    isRecognized name =
      let a = takeExtension name == ".hoo"
          b = "lts-" `isPrefixOf` name
      in a && b



-- turn string "print :: Show a => a -> IO ()"
-- into ("print :: a -> IO ()", "Show a")
splitTypeConstraint :: String -> Maybe (String, String)
splitTypeConstraint fullSig =
  let colon = " :: "
      arrow = " => "
  in do
      (name, afterName) <- stripInfix colon fullSig
      (typeConstraint, smallSig) <- stripInfix arrow afterName
      return (name ++ colon ++ smallSig, typeConstraint)

-- BEGIN d419e005-b736-4dee-8019-4c0bd7851320
--
-- These functions are copied from the Hoogle code base:
--
--     Repository : https://github.com/ndmitchell/hoogle
--     Version    : Version 5.0.14
--     Commit     : b52d3d8b6a7a1e2405c0c5dc4e02d85e21a234e3
--     File       : src/General/Util.hs
--
-- Modifications are made, the modified codes are licensed under the BSD license.

unescapeHTML :: String -> String
unescapeHTML ('&':xs)
    | Just ys <- stripPrefix "lt;" xs = '<' : unescapeHTML ys
    | Just ys <- stripPrefix "gt;" xs = '>' : unescapeHTML ys
    | Just ys <- stripPrefix "amp;" xs = '&' : unescapeHTML ys
    | Just ys <- stripPrefix "quot;" xs = '\"' : unescapeHTML ys
unescapeHTML (x:xs) = x : unescapeHTML xs
unescapeHTML [] = []

innerTextHTML :: String -> String
innerTextHTML ('<':xs) = innerTextHTML $ drop 1 $ dropWhile (/= '>') xs
innerTextHTML (x:xs) = x : innerTextHTML xs
innerTextHTML [] = []

unHTML :: String -> String
unHTML = unescapeHTML . innerTextHTML

-- END d419e005-b736-4dee-8019-4c0bd7851320
