pragma Singleton
import QtQuick 2.9


QtObject {
    readonly property color normalBg: "#fdf6e3"
    readonly property color normalFg: "#657b83"
    readonly property color selectedBg: "#586e75"
    readonly property color selectedFg: "#fdf6e3"
    readonly property color lightFg: "#747f83"
    readonly property color inputBorder: "#586e75"

    readonly property font searchFont:
    Qt.font({
                pointSize: 12,
                family: "Input Mono"})

    readonly property font matchMainFont:
    Qt.font({
                pointSize: 10,
                family: "Input Mono, Light",
                lineHeight: 1})

    readonly property font matchMainFontHoogle:
    Qt.font({
                pointSize: 10,
                family: "Input Mono, Light",
                lineHeight: 1})

    readonly property font matchMetaFont:
    Qt.font({
                pointSize: 8,
                family: "Input Mono, Light",
                lineHeight: 1})

    readonly property font matchVersionFont:
    Qt.font({
                pointSize: 6,
                lineHeight: 1})

    readonly property font matchShortcutFont:
    Qt.font({
                pointSize: 8,
                family: "Input Mono, Light"})

    readonly property int ewPadding: 8
    readonly property int nsPadding: 4
}
