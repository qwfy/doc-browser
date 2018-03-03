pragma Singleton
import QtQuick 2.9


QtObject {
    readonly property color normalBg: "#fdf6e3"
    readonly property color normalFg: "#657b83"
    readonly property color selectedBg: "#586e75"
    readonly property color selectedFg: "#fdf6e3"

    readonly property font searchFont:
    Qt.font({
                pixelSize: 24,
                family: "Input Mono"})

    readonly property font matchMainFont:
    Qt.font({
                pixelSize: 22,
                family: "Input Mono, Light",
                lineHeight: 1})

    readonly property font matchShortcutFont:
    Qt.font({
                pixelSize: 17,
                family: "Input Mono, Light"})

    readonly property int ewPadding: 8
}
