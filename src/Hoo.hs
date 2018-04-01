{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findDatabase
  , install
  ) where

import Data.List.Extra
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Safe
import System.FilePath
import System.Directory.Extra

import Control.Monad
import Control.Exception

import qualified Hoogle

import qualified Match
import qualified Doc
import Utils

search :: FilePath -> Doc.Version -> Hoogle.Database -> Int -> String -> (String -> Text) -> [Match.T]
search configRoot version db limit query prefixHost =
  Hoogle.searchDatabase db query
  |> take limit
  |> map (toMatch prefixHost configRoot version)

toMatch :: (String -> Text) -> FilePath -> Doc.Version -> Hoogle.Target -> Match.T
toMatch prefixHost configRoot version target =
  let name' = unHTML . Hoogle.targetItem $ target
      (name, typeConstraint) = maybe
        (name', Nothing)
        (\(a, b) -> (a, Just b))
        (splitTypeConstraint name')
  in Match.T { Match.collection = "Haskell"
             , Match.version = Text.pack $ Doc.getVersion version
             , Match.name = Text.pack name
             , Match.url = prefixHost . toUrl configRoot . Hoogle.targetURL $ target
             , Match.vendor = Text.pack . show $ Doc.Hoogle
             , Match.package_ = Text.pack . fst <$> Hoogle.targetPackage target
             , Match.module_ = Text.pack . fst <$> Hoogle.targetModule target
             , Match.typeConstraint = Text.pack <$> typeConstraint
             }

toUrl :: FilePath -> String -> String
toUrl configRoot prefixedPath =
  let prefix = "file://" ++ addTrailingPathSeparator configRoot
      -- TODO @incomplete: This 404 is also 404
  in fromMaybe "404.html" $ stripPrefix prefix prefixedPath


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
        |> lastMay
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

data RenameDirection = AB | BA

install :: FilePath -> FilePath -> String -> String -> IO ()
install configRoot cacheRoot url collection = do

  let docRoot = configRoot </> show Doc.Hoogle
  let cachePath = joinPath [cacheRoot, collection] <.> "tar.xz"
  archivePath <- getArchivePath url cachePath

  let unpackPath = joinPath [docRoot, collection]
  report ["unpacking", archivePath, "into", unpackPath]
  report ["this may take a while"]
  unpackXzInto archivePath unpackPath

  let runHoogle = do
        let dbPath = joinPath [docRoot, collection] <.> "hoo"
        report ["generating Hoogle database to", dbPath]
        -- TODO @incomplete: Do we need to quote dbPath and unpackPath
        -- when passing them to hoogle? The string concatenation scares me.
        Hoogle.hoogle
          [ "generate"
          , "--database=" ++ dbPath
          , "--local=" ++ unpackPath
          ]

  let renameTxts direction files = do
        let suffix = "__co.aixon.docbrowser-tempfile__"
        case direction of
          AB -> report ["temporarily relocate x to x." ++ suffix ++ ", for x in:"]
          BA -> report ["move x." ++ suffix ++ " back to x, for x in:"]
        forM_ files (\orig -> do
          report [orig]
          case direction of
            AB -> renameFile orig (orig <.> suffix)
            BA -> renameFile (orig <.> suffix) orig)

  let setup = do
        txts <- findTxtButNotHoogleTxt unpackPath
        renameTxts AB txts
        return txts

  let tearDown = renameTxts BA

  bracket setup tearDown (const runHoogle)

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

findTxtButNotHoogleTxt dir = do
  files <- listFilesRecursive dir
  return $ filter (\f -> isTxt f && not (isHoogleTxt f)) files
  where
    isTxt = (== ".txt") . takeExtension
    isHoogleTxt path =
      case reverse . splitDirectories $ path of
        (pkgTxt:pkgVer:_) ->
          case stripInfixEnd "-" pkgVer of
            Nothing ->
              False
            Just (pkg, ver) ->
              let pkg' = takeBaseName pkgTxt
                  a = pkg == pkg'
                  b = isVersionNumber ver
              in  isTxt pkgTxt && a && b
        _ ->
          False
    isVersionNumber str =
      let numbers = "0123456789"
          goodChars = "." ++ numbers
          beginWithNumber = Just True == ((`elem` numbers) <$> headMay str)
          endWithNumber = Just True == ((`elem` numbers) <$> lastMay str)
          allGoodChars = all (`elem` goodChars) str
      in beginWithNumber && endWithNumber && allGoodChars
