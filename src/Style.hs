{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Style
  ( createQml
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Shakespeare.Text

import Data.List.Extra
import qualified Data.ByteString as BS

import Path
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory

import qualified Config

createQml :: Path Abs Dir -> Config.T -> IO ()
createQml parentDir config = do
  let qml = render config
  let parts = ["co", "aixon", "docbrowser"]
  let dirPath = FilePath.joinPath $ toFilePath parentDir : parts
  Directory.createDirectoryIfMissing True dirPath
  BS.writeFile (dirPath FilePath.</> "Style.qml") (Text.encodeUtf8 qml)
  BS.writeFile (dirPath FilePath.</> "qmldir") . Text.encodeUtf8 . Text.pack . unlines $
    [ "module " ++ intercalate "." parts
    , "singleton Style 1.0 Style.qml"
    ]

render :: Config.T -> Text
render config = [st|
pragma Singleton
import QtQuick 2.9

QtObject {
    // Color used by Match.qml
    readonly property color normalBg:     "#{ Config.matchBgColorNormal config }"
    readonly property color normalFg:     "#{ Config.matchFgColorNormal config }"
    readonly property color selectedBg:   "#{ Config.matchBgColorSelected config }"
    readonly property color selectedFg:   "#{ Config.matchFgColorSelected config }"
    readonly property color lightFg:      "#{ Config.matchFgColorLight config }"

    readonly property color inputBorder: "#{ Config.inputBorderColor config }"

    readonly property font searchFont:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.inputFont config },
                family:    "#{ Config.family $ Config.inputFont config }"})

    readonly property font matchMainFont:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.matchFontMain config },
                family:    "#{ Config.family $ Config.matchFontMain config }",
                lineHeight: 1})

    readonly property font matchMainFontHoogle:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.matchFontMainHoogle config },
                family:    "#{ Config.family $ Config.matchFontMainHoogle config }",
                lineHeight: 1})

    readonly property font matchMetaFont:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.matchFontMeta config },
                family:    "#{ Config.family $ Config.matchFontMeta config }",
                lineHeight: 1})

    readonly property font matchVersionFont:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.matchFontVersion config },
                family:    "#{ Config.family $ Config.matchFontVersion config }",
                lineHeight: 1})

    readonly property font matchShortcutFont:
    Qt.font({
                pointSize: #{ show $ Config.pointSize $ Config.matchFontShortcut config },
                family:    "#{ Config.family $ Config.matchFontShortcut config }",
                lineHeight: 1})

    readonly property int ewPadding: 8
    readonly property int nsPadding: 4

    readonly property real leftColumnWidth: #{ show $ Config.leftColumnWidth config }
    readonly property real leftColumnWidthHoogle: #{ show $ Config.leftColumnWidthHoogle config }

    readonly property real webEngineZoomFactor: #{ show $ Config.webEngineZoomFactor config }
}|]
