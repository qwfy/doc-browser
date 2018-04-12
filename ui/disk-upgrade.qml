import QtQuick 2.9
import QtQuick.Window 2.2
import QtQuick.Controls 1.4

Window {
    id: root
    title: "Upgrading Doc Browser's Disk Format"
    visible: true
    width: 800
    height: 800

    ScrollView {
        anchors.fill: parent
        Text {
            width: root.width
            padding: 10
            wrapMode: Text.Wrap
            text: messages
            lineHeight: 1.2
            font.pixelSize: 20
            font.family: "Input Mono, Light"
        }
    }
}
