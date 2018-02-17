pragma Singleton
import QtQuick 2.9


QtObject {
    readonly property color normalBg: "#fdf6e3"
    readonly property color normalFg: "#657b83"
    readonly property color selectedBg: "#586e75"
    readonly property color selectedFg: "#fdf6e3"

    readonly property real h: 40

    property font searchFont:
    Qt.font({
                pixelSize: 24,
                family: "Input Mono"})

    property font matchMainFont:
    Qt.font({
                pixelSize: 24,
                family: "Input Mono"})

    property font matchShortcutFont:
    Qt.font({
                pixelSize: 14,
                family: "Input Mono"})

    readonly property int ewPadding: 8
}
