import QtQuick 2.9
import QtQuick.Layouts 1.2
import "./style"
import "match.js" as MatchJs

Rectangle {
    id: root

    anchors.left: parent.left
    anchors.right: parent.right

    // TODO @incomplete: for some unknown reason, this cannot be from a variable
    height: 40
    radius: 3.8

    signal clicked()

    property bool isSelected
    focus: isSelected
    color: isSelected ? Style.selectedBg : Style.normalBg

    MouseArea {
        anchors.fill: parent
        onClicked: root.clicked()
    }

    Keys.onPressed: {
        if (event.key === Qt.Key_Return) {
            root.clicked();
            event.accepted = true;
        }
    }

    Item {
        anchors.fill: parent
        anchors.leftMargin: Style.ewPadding
        anchors.rightMargin: Style.ewPadding

        Text {
            id: shortcut
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            text: MatchJs.shortcutText(index)
            font: Style.matchShortcutFont
            color: root.isSelected ? Style.selectedFg : MatchJs.shortcutColor(index)
        }

        Image {
            id: icon
            anchors.left: shortcut.right
            anchors.leftMargin: Style.ewPadding
            anchors.verticalCenter: parent.verticalCenter
            source: MatchJs.icon(modelData.language)
        }

        Text {
            id: mainText
            clip: true
            anchors.left: icon.right
            anchors.leftMargin: Style.ewPadding
            anchors.right: parent.right
            anchors.rightMargin: Style.ewPadding
            anchors.verticalCenter: parent.verticalCenter

            text: modelData.name

            font: Style.matchMainFont
            color: root.isSelected ? Style.selectedFg : Style.normalFg
        }


    }
}
