import QtQuick 2.9
import QtQuick.Layouts 1.2
import QtWebEngine 1.5
import QtQuick.Window 2.2
import QtQuick.Controls 1.4
import QtLocation 5.3

import co.aixon.docbrowser 1.0
import "main.js" as Logic
import "match.js" as MatchJs
import "tab.js" as TabJs
import "util.js" as UtilJs

Window {
    id: rootWindow
    title: Logic.windowTitle()
    visible: true
    visibility: Window.Maximized

    // wait for the Haskell side to return the search result.
    // purpose: open the first match on HTTP summon
    property bool waitingForSummonResult: false

    Connections {
        target: controller
        onSummon: {
            if (controller.summonText !== "") {
                searchInput.text = controller.summonText;
                rootWindow.waitingForSummonResult = true;

                // TODO @incomplete: handle this properly
                rootWindow.visible = true;
                rootWindow.visibility = Window.Maximized;
                if (focusOnSummon) {
                    rootWindow.raise();
                    rootWindow.requestActivate();
                } else {
                    alert(5000);
                }
            }
        }
    }

    Shortcut {
        sequence: "/"
        onActivated: searchInput.focus = true
    }

    Shortcut {
        sequence: "y"
        onActivated: {
            var match = matchContainer.model[matchContainer.selected];
            if (match) {
                setClipboard(match.name);
            }
        }
    }

    Shortcut {
        sequence: "m"
        onActivated: {
            var match = matchContainer.model[matchContainer.selected];
            if (match) {
                if (UtilJs.isString(match.module_) && match.module_ !== "") {
                    setClipboard(match.module_);
                }
            }
        }
    }

    Shortcut {
        sequence: "p"
        onActivated: {
            var match = matchContainer.model[matchContainer.selected];
            if (match) {
                if (UtilJs.isString(match.package_) && match.package_ !== "") {
                    var t = match.package_;
                    if (match.version !== "") {
                        t += "-";
                        t += match.version;
                    }
                    setClipboard(t);
                }
            }
        }
    }

    function togglePageSearch() {
        pageSearch.visible = ! pageSearch.visible;
        if (pageSearch.visible) {
            tabContainer.anchors.top = pageSearch.bottom;
            tabContainer.anchors.topMargin = Style.nsPadding;
            pageSearchInput.focus = true;
            pageSearchInput.selectAll();
        } else {
            tabContainer.anchors.top = rightColumn.top;
            tabContainer.anchors.topMargin = 0;
            pageSearchInput.text = "";
        }
    }

    function closePaseSearch() {
        togglePageSearch();
        Logic.clearPageSearch();
        pageSearchInput.focus = false;
        pageSearchInputContainer.color = "white";
    }

    Shortcut {
        sequence: "Ctrl+f"
        onActivated: {
            if (pageSearch.visible) {
                pageSearchInput.focus = true;
                pageSearchInput.selectAll();
            } else {
                togglePageSearch();
            }
        }
    }

    Shortcut {
        sequence: "Ctrl+/"
        onActivated: google(searchInput.text);
    }

    Shortcut {
        sequence: "Ctrl+i"
        onActivated: {
            if (!searchInput.focus) {
                searchInput.focus = true;
            }
            var len = searchInput.text.length;
            var txt = searchInput.text;
            if (len >= 3) {
                if (txt.startsWith("/")) {
                    searchInput.text = txt.substring(0, 3);
                } else if (txt[len-3] === "/") {
                    searchInput.text = txt.substring(len-3, len);
                } else {
                    searchInput.text = ""
                }
            } else {
                searchInput.text = "";
            }
        }
    }

    // =====================================================================
    // Shortcuts for navigating matches
    // TODO @incomplete: is this insane?
    // =====================================================================
    Shortcut {sequence: MatchJs.shortcut( 1 - 1); onActivated: Logic.matchOpen( 1 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 2 - 1); onActivated: Logic.matchOpen( 2 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 3 - 1); onActivated: Logic.matchOpen( 3 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 4 - 1); onActivated: Logic.matchOpen( 4 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 5 - 1); onActivated: Logic.matchOpen( 5 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 6 - 1); onActivated: Logic.matchOpen( 6 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 7 - 1); onActivated: Logic.matchOpen( 7 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 8 - 1); onActivated: Logic.matchOpen( 8 - 1)}
    Shortcut {sequence: MatchJs.shortcut( 9 - 1); onActivated: Logic.matchOpen( 9 - 1)}
    Shortcut {sequence: MatchJs.shortcut(10 - 1); onActivated: Logic.matchOpen(10 - 1)}
    Shortcut {sequence: MatchJs.shortcut(11 - 1); onActivated: Logic.matchOpen(11 - 1)}
    Shortcut {sequence: MatchJs.shortcut(12 - 1); onActivated: Logic.matchOpen(12 - 1)}
    Shortcut {sequence: MatchJs.shortcut(13 - 1); onActivated: Logic.matchOpen(13 - 1)}
    Shortcut {sequence: MatchJs.shortcut(14 - 1); onActivated: Logic.matchOpen(14 - 1)}
    Shortcut {sequence: MatchJs.shortcut(15 - 1); onActivated: Logic.matchOpen(15 - 1)}
    Shortcut {sequence: MatchJs.shortcut(16 - 1); onActivated: Logic.matchOpen(16 - 1)}
    Shortcut {sequence: MatchJs.shortcut(17 - 1); onActivated: Logic.matchOpen(17 - 1)}
    Shortcut {sequence: MatchJs.shortcut(18 - 1); onActivated: Logic.matchOpen(18 - 1)}
    Shortcut {sequence: MatchJs.shortcut(19 - 1); onActivated: Logic.matchOpen(19 - 1)}
    Shortcut {sequence: MatchJs.shortcut(20 - 1); onActivated: Logic.matchOpen(20 - 1)}
    Shortcut {sequence: MatchJs.shortcut(21 - 1); onActivated: Logic.matchOpen(21 - 1)}
    Shortcut {sequence: MatchJs.shortcut(22 - 1); onActivated: Logic.matchOpen(22 - 1)}
    Shortcut {sequence: MatchJs.shortcut(23 - 1); onActivated: Logic.matchOpen(23 - 1)}
    Shortcut {sequence: MatchJs.shortcut(24 - 1); onActivated: Logic.matchOpen(24 - 1)}
    Shortcut {sequence: MatchJs.shortcut(25 - 1); onActivated: Logic.matchOpen(25 - 1)}
    Shortcut {sequence: MatchJs.shortcut(26 - 1); onActivated: Logic.matchOpen(26 - 1)}
    Shortcut {sequence: MatchJs.shortcut(27 - 1); onActivated: Logic.matchOpen(27 - 1)}

    Shortcut {sequence: "j"; onActivated: {Logic.matchSelect(matchContainer.selected + 1)}}
    Shortcut {sequence: "k"; onActivated: {Logic.matchSelect(matchContainer.selected - 1)}}


    // =====================================================================
    // Shortcuts for navigating tabs
    // =====================================================================
    Shortcut {sequence: TabJs.keys[ 1 - 1]; onActivated: Logic.tabSelect( 1 - 1)}
    Shortcut {sequence: TabJs.keys[ 2 - 1]; onActivated: Logic.tabSelect( 2 - 1)}
    Shortcut {sequence: TabJs.keys[ 3 - 1]; onActivated: Logic.tabSelect( 3 - 1)}
    Shortcut {sequence: TabJs.keys[ 4 - 1]; onActivated: Logic.tabSelect( 4 - 1)}
    Shortcut {sequence: TabJs.keys[ 5 - 1]; onActivated: Logic.tabSelect( 5 - 1)}
    Shortcut {sequence: TabJs.keys[ 6 - 1]; onActivated: Logic.tabSelect( 6 - 1)}
    Shortcut {sequence: TabJs.keys[ 7 - 1]; onActivated: Logic.tabSelect( 7 - 1)}
    Shortcut {sequence: TabJs.keys[ 8 - 1]; onActivated: Logic.tabSelect( 8 - 1)}
    Shortcut {sequence: TabJs.keys[ 9 - 1]; onActivated: Logic.tabSelect( 9 - 1)}
    Shortcut {sequence: TabJs.keys[10 - 1]; onActivated: Logic.tabSelect(10 - 1)}

    Shortcut {sequence: "Alt+l"; onActivated: {Logic.tabSelect(tabContainer.currentIndex + 1)}}
    Shortcut {sequence: "Alt+h"; onActivated: {Logic.tabSelect(tabContainer.currentIndex - 1)}}

    Shortcut {sequence: "Ctrl+w"; onActivated: {Logic.tabClose(tabContainer.currentIndex)}}


    RowLayout {
        anchors.fill: parent

        ColumnLayout {
            id: leftColumn

            anchors.top: parent.top
            anchors.left: parent.left
            anchors.bottom: parent.bottom
            Layout.fillWidth: false
            Layout.preferredWidth: Logic.isHoogleMode() ? Style.leftColumnWidthHoogle : Style.leftColumnWidth

            spacing: 0

            Rectangle {
                id: searchInputContainer

                border.width: 1
                border.color: Style.inputBorder
                radius: 3

                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right

                anchors.topMargin: searchInput.font.pointSize
                anchors.leftMargin: searchInput.font.pointSize
                anchors.rightMargin: searchInput.font.pointSize

                height: searchInput.height
                clip: true

                TextInput {
                    id: searchInput

                    anchors.verticalCenter: parent.verticalCenter
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.leftMargin: Style.ewPadding
                    anchors.rightMargin: Style.ewPadding

                    topPadding: font.pointSize * 0.6
                    bottomPadding: font.pointSize * 0.6

                    // when the application started, this field will have focus
                    focus: true

                    selectByMouse: true

                    font: Style.searchFont

                    onTextChanged: {
                        // TODO @incomplete:
                        //
                        // Ideally, matchContainer.selected should be set to -1 when
                        // onMatchesChanged, to reflect the fact that the selection
                        // is no longer correct due to the change of the matches.
                        //
                        // But I haven't found a way to do this, as an approximation,
                        // do it here, which assumes that whenever the text changed,
                        // so does the matches. This assumption is not always true,
                        // when it is false, we suffer from a bad performance, but
                        // the functionality is still correct.
                        if (matchContainer.selected !== -1) matchContainer.selected = -1;

                        search(text);
                    }

                    onAccepted: Logic.matchSelect(0)

                    Keys.onEscapePressed: focus = false
                    Keys.onDownPressed: accepted()
                }
            }


            ScrollView {
                id: matchContainerFocusScope

                anchors.top: searchInputContainer.bottom
                anchors.topMargin: searchInput.font.pointSize
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                ColumnLayout {

                    spacing: 1
                    width: matchContainerFocusScope.viewport.width

                    Repeater {
                        id: matchContainer

                        model: matches

                        onModelChanged: {
                            if (waitingForSummonResult) {
                                waitingForSummonResult = false;
                                Logic.matchOpen(0);
                            }
                        }

                        property int selected: -1

                        Match {
                            isSelected: index === matchContainer.selected
                            onClicked: Logic.matchOpen(index)
                            Keys.onPressed: {
                                if (event.key === Qt.Key_Tab || event.key === Qt.Key_Down) {
                                    Logic.matchSelect(index + 1);
                                    event.accepted = true;
                                } else if (event.key === Qt.Key_Backtab || event.key === Qt.Key_Up) {
                                    Logic.matchSelect(index - 1);
                                    event.accepted = true;
                                }
                            }
                        }

                    }
                }
            }

        }


        ColumnLayout {
            id: rightColumn
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.left: leftColumn.right
            anchors.right: parent.right

            RowLayout {
                id: pageSearch
                visible: false
                clip: true
                anchors.top: parent.top
                anchors.topMargin: Style.nsPadding
                anchors.left: parent.left
                anchors.right: parent.right
                Button {
                    text: "close"
                    onClicked: closePaseSearch()
                }
                Button {
                    text: "&previous"
                    onClicked: Logic.searchCurrentPage(pageSearchInput.text, WebEngineView.FindBackward)
                }
                Button {
                    id: pageSearchButtonNext
                    text: "&next"
                    onClicked: Logic.searchCurrentPage(pageSearchInput.text)
                }
                Rectangle{
                    id: pageSearchInputContainer

                    border.width: 1
                    border.color: Style.inputBorder
                    radius: 3

                    anchors.left: pageSearchButtonNext.right
                    anchors.right: parent.right
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    anchors.leftMargin: Style.ewPadding
                    anchors.rightMargin: Style.ewPadding

                    TextInput {
                        id: pageSearchInput
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: Style.ewPadding
                        anchors.rightMargin: Style.ewPadding
                        font: Style.searchFont
                        onAccepted: Logic.searchCurrentPage(text)
                        Keys.onEscapePressed: closePaseSearch()
                        onTextChanged: pageSearchInputContainer.color = "white"
                        selectByMouse: true
                    }
                }
            }

            TabView {
                id: tabContainer
                anchors.top: rightColumn.top
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
            }
        }

    }
}
