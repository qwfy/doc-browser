{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findDatabase
  , install
  ) where

import qualified Safe
import Data.List.Extra
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Control.Monad
import Data.Char

import qualified Hoogle

import qualified Match
import qualified Doc
import Utils

search :: Doc.Version -> Hoogle.Database -> Int -> String -> [Match.T]
search version db limit query =
  Hoogle.searchDatabase db query
  |> take limit
  |> map (toMatch version)

toMatch :: Doc.Version -> Hoogle.Target -> Match.T
toMatch version target =
  let name' = unHTML . Hoogle.targetItem $ target
      (name, typeConstraint) = maybe
        (name', Nothing)
        (\(a, b) -> (a, Just b))
        (splitTypeConstraint name')
  in Match.T { Match.collection = "Haskell"
             -- TODO @incomplete: don't hard code this
             , Match.version = Text.pack $ Doc.getVersion version
             , Match.name = Text.pack name
             , Match.url = Text.pack $ Hoogle.targetURL target
             , Match.vendor = Text.pack . show $ Doc.Hoogle
             , Match.package_ = Text.pack . fst <$> Hoogle.targetPackage target
             , Match.module_ = Text.pack . fst <$> Hoogle.targetModule target
             , Match.typeConstraint = Text.pack <$> typeConstraint
             }


findDatabase :: FilePath -> IO (Maybe FilePath)
findDatabase configRoot = do
  let hoogleDir = joinPath [configRoot, show Doc.Hoogle]
  exist <- doesDirectoryExist hoogleDir
  if not exist
    then return Nothing
    else do
      paths <- listDirectory hoogleDir >>= filterM (doesFileExist . (hoogleDir </>))
      filter isRecognized paths
        |> sort
        -- currently, only load the latest
        |> Safe.lastMay
        |> fmap (show Doc.Hoogle </>)
        |> return
  where
    isRecognized name = takeExtension name == ".hoo"


-- turn string "print :: Show a => a -> IO ()"
-- into ("print :: a -> IO ()", "Show a")
splitTypeConstraint :: String -> Maybe (String, String)
splitTypeConstraint fullSig =
  let colon = " :: "
      arrow = " => "
      arrow' = " =>"
  in do
      (name, afterName) <- stripInfix colon fullSig
      (typeConstraint, smallSig) <- stripInfixEnd arrow afterName
      return (name ++ colon ++ smallSig, typeConstraint ++ arrow')

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


install :: FilePath -> FilePath -> String -> String -> IO ()
install configRoot cacheRoot url collection = do

  let docRoot = configRoot </> show Doc.Hoogle
  let cachePath = joinPath [cacheRoot, collection] <.> "tar.xz"
  archivePath <- getArchivePath url cachePath

  let unpackPath = joinPath [docRoot, collection]
  report ["unpacking", archivePath, "into", unpackPath]
  unpackXzInto archivePath unpackPath

  let dbPath = joinPath [docRoot, collection] <.> "hoo"
  report ["generating Hoogle database to", dbPath]
  -- TODO @incomplete: Do we need to quote dbPath and unpackPath
  -- when passing them to hoogle? The string concatenation scares me.
  Hoogle.hoogle
    [ "generate"
    , "--database=" ++ dbPath
    , "--local=" ++ unpackPath
    ]

  report ["Hoogle database generated"]

isHttp str =
  let lowerStr = map toLower str
  in "http://" `isPrefixOf` lowerStr || "https://" `isPrefixOf` lowerStr

-- TODO @incomplete: verify integrity
getArchivePath url cachePath
  | isHttp url = do
    cached <- doesFileExist cachePath
    unless cached $ do
      report ["downloading", url, "to", cachePath]
      downloadFile' url cachePath
    return cachePath
  | otherwise =
    -- It's the user's fault if this file does not exist.
    return url
