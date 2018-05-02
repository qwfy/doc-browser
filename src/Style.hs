{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Style
  ( createQml
  ) where

import Control.Monad

import Data.Aeson
import Text.Mustache
import Text.Mustache.Compile.TH (mustache)
import qualified Data.Text.Lazy.IO as LTIO
import Data.List.Extra

import Path
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory

import qualified Config

createQml :: Path Abs Dir -> Config.T -> IO ()
createQml parentDir config = do
  let (warnings, qml) = renderMustacheW template (toJSON config)
  unless (null warnings) (print ("WARNING", warnings))

  let parts = ["co", "aixon", "docbrowser"]
  let dirPath = FilePath.joinPath $ toFilePath parentDir : parts
  Directory.createDirectoryIfMissing True dirPath
  LTIO.writeFile (dirPath FilePath.</> "Style.qml") qml
  writeFile (dirPath FilePath.</> "qmldir") $ unlines
    [ "module " ++ intercalate "." parts
    , "singleton Style 1.0 Style.qml"
    ]

template :: Template
template = [mustache|
pragma Singleton
import QtQuick 2.9

QtObject {
    // Color used by Match.qml
    readonly property color normalBg:     "{{ matchBgColorNormal }}"
    readonly property color normalFg:     "{{ matchFgColorNormal }}"
    readonly property color selectedBg:   "{{ matchBgColorSelected }}"
    readonly property color selectedFg:   "{{ matchFgColorSelected }}"
    readonly property color lightFg:      "{{ matchFgColorLight }}"

    readonly property color inputBorder: "{{ inputBorderColor }}"

    readonly property font searchFont:
    Qt.font({
                pointSize: {{ inputFont.pointSize }},
                family:    "{{ inputFont.family }}"})

    readonly property font matchMainFont:
    Qt.font({
                pointSize: {{ matchFontMain.pointSize }},
                family:    "{{ matchFontMain.family }}",
                lineHeight: 1})

    readonly property font matchMainFontHoogle:
    Qt.font({
                pointSize: {{ matchFontMainHoogle.pointSize }},
                family:    "{{ matchFontMainHoogle.family }}",
                lineHeight: 1})

    readonly property font matchMetaFont:
    Qt.font({
                pointSize: {{ matchFontMeta.pointSize }},
                family:    "{{ matchFontMeta.family }}",
                lineHeight: 1})

    readonly property font matchVersionFont:
    Qt.font({
                pointSize: {{ matchFontVersion.pointSize }},
                family:    "{{ matchFontVersion.family }}",
                lineHeight: 1})

    readonly property font matchShortcutFont:
    Qt.font({
                pointSize: {{ matchFontShortcut.pointSize }},
                family:    "{{ matchFontShortcut.family }}",
                lineHeight: 1})

    readonly property int ewPadding: 8
    readonly property int nsPadding: 4

    readonly property real leftColumnWidth: {{ leftColumnWidth }}
    readonly property real leftColumnWidthHoogle: {{ leftColumnWidthHoogle }}

    readonly property real webEngineZoomFactor: {{ webEngineZoomFactor }}
}|]
