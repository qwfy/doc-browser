{-# LANGUAGE OverloadedStrings #-}

module Hoo
  ( search
  , findLatest
  , findDatabases
  , findDatabasesAsMap
  , extractCollection
  , install
  , installFromDir
  ) where

import Data.List.Extra
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Safe

import Control.Monad
import Control.Exception
import Control.Monad.Catch

import Path
import Path.IO
import qualified System.FilePath as FilePath
import qualified Hoogle

import qualified Match
import qualified Doc
import Utils

search :: ConfigRoot -> Doc.Collection -> Hoogle.Database -> Int -> String -> (String -> Text) -> [Match.T]
search configRoot collection db limit query prefixHost =
  Hoogle.searchDatabase db query
  |> take limit
  |> map (toMatch prefixHost configRoot collection)

toMatch :: (String -> Text) -> ConfigRoot -> Doc.Collection -> Hoogle.Target -> Match.T
toMatch prefixHost configRoot collection target =
  let name' = unHTML . Hoogle.targetItem $ target
      (name, typeConstraint) = maybe
        (name', Nothing)
        (\(a, b) -> (a, Just b))
        (splitTypeConstraint name')
  in Match.T { Match.collection = Text.pack $ Doc.getCollection collection
             , Match.version = maybe "" Text.pack $ getPackageVersion configRoot target collection
             , Match.name = Text.pack name
             , Match.url = prefixHost . removeProtocolAndLeading configRoot . Hoogle.targetURL $ target
             , Match.vendor = Text.pack . show $ Doc.Hoogle
             , Match.package_ = Text.pack . fst <$> Hoogle.targetPackage target
             , Match.module_ = Text.pack . fst <$> Hoogle.targetModule target
             , Match.typeConstraint = Text.pack <$> typeConstraint
             , Match.icon = Just "Haskell"
             }

removeProtocolAndLeading :: ConfigRoot -> String -> String
removeProtocolAndLeading configRoot pathWithProtocol =
  let prefix = "file://" ++ toFilePath (getConfigRoot configRoot)
  in fromMaybe pathWithProtocol $ stripPrefix prefix pathWithProtocol

getPackageVersion :: ConfigRoot -> Hoogle.Target -> Doc.Collection -> Maybe String
getPackageVersion configRoot target collection = do
  (package, url') <- Hoogle.targetPackage target
  -- TODO @incomplete: this dropWhile is not cross platform
  let url = dropWhile (== '/') $ removeProtocolAndLeading configRoot url'
  let urls = FilePath.splitDirectories url
  case urls of
    (vnd : coll : pkgVer : _) | vnd == show Doc.Hoogle, coll == show collection ->
      pkgVer `isPkgVerOfPkg` package
    _ ->
      Nothing

findLatest :: ConfigRoot -> IO (Maybe (Path Abs File))
findLatest configRoot = do
  databaseFiles <- findDatabases configRoot
  databaseFiles
    |> sort
    |> lastMay
    |> return

findDatabases :: ConfigRoot -> IO ([Path Abs File])
findDatabases configRoot = do
  hoogle <- (getConfigRoot configRoot </>) <$> parseRelDir (show Doc.Hoogle)
  exist <- doesDirExist hoogle
  if not exist
    then return []
    else do
      (_dirs, files) <- listDir hoogle
      filter isRecognized files
        |> return
  where
    isRecognized name = fileExtension name == ".hoo"

findDatabasesAsMap :: ConfigRoot -> IO (Map Doc.Collection (Path Abs File))
findDatabasesAsMap configRoot = do
  dbs <- findDatabases configRoot
  colls <- sequence $ map extractCollection dbs
  return . Map.fromList $ zip colls dbs


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
install :: ConfigRoot -> CacheRoot -> String -> Doc.Collection -> IO ()
install configRoot cacheRoot url collection'' = do
  collection <- parseRelFile $ Doc.getCollection collection''
  collection' <- parseRelDir $ Doc.getCollection collection''

  docRoot <- (getConfigRoot configRoot </>) <$> parseRelDir (show Doc.Hoogle)
  cachePath <- (getCacheRoot cacheRoot </> collection) <.> "tar.xz"
  archivePath <- getArchivePath url cachePath

  let unpackPath = docRoot </> collection'
  report ["unpacking", toFilePath archivePath, "into", toFilePath unpackPath ++ ",", "this may take a while"]
  unpackXzInto archivePath unpackPath

  installFromDir unpackPath

installFromDir :: Path Abs Dir -> IO ()
installFromDir unpackPath = do
  let runHoogle = do
        dbPath <- extractAp (<.>) (parseAbsFile . FilePath.dropTrailingPathSeparator . fromAbsDir $ unpackPath) "hoo"
        report ["generating Hoogle database to", toFilePath dbPath]
        Hoogle.hoogle
          [ "generate"
          , "--database=" ++ toFilePath dbPath
          , "--local=" ++ toFilePath unpackPath
          ]

  let renameTxts direction files
        | null files = return ()
        | otherwise = do
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

  Control.Exception.bracket setup tearDown (const runHoogle)

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
      downloadFile url cachePath
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
          isTxt pkgTxt && (isJust $ pkgVer `isPkgVerOfPkg` FilePath.takeBaseName pkgTxt)
        _ ->
          False

isPkgVerOfPkg :: String -> String -> Maybe String
pkgVer `isPkgVerOfPkg` wantPkg =
  case stripInfixEnd "-" pkgVer of
    Nothing ->
      Nothing
    Just (pkg, ver) ->
      case (pkg == wantPkg) && (isVersionNumber ver) of
        True -> Just ver
        False -> Nothing
  where
    isVersionNumber str =
      let numbers = "0123456789"
          goodChars = "." ++ numbers
          beginWithNumber = Just True == ((`elem` numbers) <$> headMay str)
          endWithNumber = Just True == ((`elem` numbers) <$> lastMay str)
          allGoodChars = all (`elem` goodChars) str
      in beginWithNumber && endWithNumber && allGoodChars

extractCollection :: MonadThrow m => Path Abs File -> m Doc.Collection
extractCollection path =
  Doc.parseCollection . FilePath.takeBaseName . toFilePath $ path
