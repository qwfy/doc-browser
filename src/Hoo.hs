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

import Control.Monad
import Control.Exception

import Path
import Path.IO
import qualified System.FilePath as FilePath
import qualified Hoogle

import qualified Match
import qualified Doc
import Utils

-- TODO @incomplete: correct this version argument, maybe change it to collection?
search :: ConfigRoot -> Doc.Version -> Hoogle.Database -> Int -> String -> (String -> Text) -> [Match.T]
search configRoot version db limit query prefixHost =
  Hoogle.searchDatabase db query
  |> take limit
  |> map (toMatch prefixHost configRoot version)

toMatch :: (String -> Text) -> ConfigRoot -> Doc.Version -> Hoogle.Target -> Match.T
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

toUrl :: ConfigRoot -> String -> String
toUrl configRoot pathWithProtocol =
  let prefix = "file://" ++ toFilePath (getConfigRoot configRoot)
      -- TODO @incomplete: This 404 is also 404
  in fromMaybe "404.html" $ stripPrefix prefix pathWithProtocol


findDatabase :: ConfigRoot -> IO (Maybe (Path Abs File))
findDatabase configRoot = do
  hoogle <- (getConfigRoot configRoot </>) <$> parseRelDir (show Doc.Hoogle)
  exist <- doesDirExist hoogle
  if not exist
    then return Nothing
    else do
      (_dirs, files) <- listDir hoogle
      filter isRecognized files
        |> sort
        -- currently, only load the latest
        |> lastMay
        |> return
  where
    isRecognized name = fileExtension name == ".hoo"


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

-- TODO @incomplete: replace collection with a proper type
install :: ConfigRoot -> CacheRoot -> String -> String -> IO ()
install configRoot cacheRoot url collection'' = do
  collection <- parseRelFile collection''
  collection' <- parseRelDir collection''

  docRoot <- (getConfigRoot configRoot </>) <$> parseRelDir (show Doc.Hoogle)
  cachePath <- (getCacheRoot cacheRoot </> collection) <.> "tar.xz"
  archivePath <- getArchivePath url cachePath

  let unpackPath = docRoot </> collection'
  report ["unpacking", toFilePath archivePath, "into", toFilePath unpackPath]
  report ["this may take a while"]
  unpackXzInto archivePath unpackPath

  let runHoogle = do
        dbPath <- (docRoot </> collection) <.> "hoo"
        report ["generating Hoogle database to", toFilePath dbPath]
        -- TODO @incomplete: Do we need to quote dbPath and unpackPath
        -- when passing them to hoogle? The string concatenation scares me.
        Hoogle.hoogle
          [ "generate"
          , "--database=" ++ toFilePath dbPath
          , "--local=" ++ toFilePath unpackPath
          ]

  let renameTxts direction files = do
        let suffix = "__co.aixon.docbrowser-tempfile__"
        case direction of
          AB -> report ["temporarily relocate x to x." ++ suffix ++ ", for x in:"]
          BA -> report ["move x." ++ suffix ++ " back to x, for x in:"]
        forM_ files (\orig -> do
          report [toFilePath orig]
          case direction of
            AB -> orig <.> suffix >>= renameFile orig
            BA -> orig <.> suffix >>= (flip renameFile) orig)

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
-- TODO @incomplete: replace url with a sum type
getArchivePath :: String -> Path Abs File -> IO (Path Abs File)
getArchivePath url cachePath
  | isHttp url = do
    cached <- doesFileExist cachePath
    unless cached $ do
      report ["downloading", url, "to", toFilePath cachePath]
      downloadFile' url cachePath
    return cachePath
  | otherwise =
    -- It's the user's fault if this file does not exist.
    resolveFile' url

-- TODO @incomplete: do this elegently
findTxtButNotHoogleTxt :: Path Abs Dir -> IO [Path Abs File]
findTxtButNotHoogleTxt dir = do
  (_dirs, files') <- listDirRecur dir
  let files = map toFilePath files'
  let wants = filter (\f -> isTxt f && not (isHoogleTxt f)) files
  mapM parseAbsFile wants
  where
    isTxt = (== ".txt") . FilePath.takeExtension
    isHoogleTxt path =
      case reverse . FilePath.splitDirectories $ path of
        (pkgTxt:pkgVer:_) ->
          case stripInfixEnd "-" pkgVer of
            Nothing ->
              False
            Just (pkg, ver) ->
              let pkg' = FilePath.takeBaseName pkgTxt
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
