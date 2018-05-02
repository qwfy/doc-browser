.import "match.js" as MatchJs
.import "tab.js" as TabJs
.import "util.js" as UtilJs


// =====================================================================
// Match manipulation
// =====================================================================

function matchBoundIndex(index) {
    return UtilJs.boundIndex(index, matchContainer.count);
}

function matchSelect(indexIn) {
    var index = matchBoundIndex(indexIn);
    if (index >= 0 && matchContainer.selected !== index) {
        matchContainerFocusScope.focus = true;
        matchContainer.selected = index;
    }
}

// Select the specified match, and open it in a new tab.
function matchOpen(indexIn) {
    var index = matchBoundIndex(indexIn);
    if (index >= 0) {
        matchSelect(index);

        // TODO @incomplete: make this number configurable
        var maxTabs = 10;

        if (tabContainer.count === maxTabs) {
            tabContainer.removeTab(0);
        }

        var tabIndex = tabContainer.count;
        var component = Qt.createComponent("Doc.qml");
        var targetTab = tabContainer.insertTab(tabIndex, "Loading", component);
        targetTab.active = true;
        targetTab.item.url = matchContainer.model[index].url;
        targetTab.item.selectedName = matchContainer.model[index].name;
        targetTab.title = Qt.binding(function(){
            var newIndex = tabIndexOf(targetTab);
            if (newIndex >= 0) {
                var newTab = tabContainer.getTab(newIndex);
                if (newTab.item !== null) {
                    return TabJs.title(newIndex, newTab.item.selectedName);
                } else {
                    return "<Tab Not Found>";
                }
            } else {
                return "<Tab Not Found>";
            }
        });
        tabContainer.currentIndex = tabIndex;
    }
}


// =====================================================================
// Tab manipulation
// =====================================================================

function tabBoundIndex(index) {
    return UtilJs.boundIndex(index, tabContainer.count);
}

function tabSelect(indexIn) {
    var index = tabBoundIndex(indexIn);
    if (index >= 0 && tabContainer.currentIndex !== index) {
        tabContainer.currentIndex = index;
        tabContainer.getTab(index).active = true;
    }
}


// TODO @incomplete: currently this will report error:
// file:///usr/lib/qt/qml/QtQuick/Controls/Styles/Desktop/TabViewStyle.qml:92:9:
// QML StyleItem: Cannot anchor to an item that isn't a parent or sibling.
function tabClose(indexIn) {
    var index = tabBoundIndex(indexIn);
    if (index >= 0) {
        var targetTab = tabContainer.getTab(index);
        // TODO @incomplete: do we need some cleaning?
        targetTab.active = false;
        tabContainer.removeTab(index);
    }
}


function tabIndexOf(tab) {
    var index = -1
    for (var i=0; i<=tabContainer.count-1; i++) {
        var targetTab = tabContainer.getTab(i);
        if (targetTab === tab) {
            index = i;
            break;
        }
    }
    return index;
}

function clearPageSearch() {
    searchCurrentPage("");
}

function searchCurrentPage(txt, opt) {
    var index = tabContainer.currentIndex;
    var opt = opt !== null ? opt : 0;
    if (index >= 0) {
        var targetTab = tabContainer.getTab(index);
        targetTab.item.findText(txt, opt,
            function(found) {
                if (txt !== "" && !found) pageSearchInputContainer.color = "red";
            });
    }
}

// =====================================================================
// Left column
// =====================================================================
function isHoogleMode() {
    var b = searchInput.text.substr(0, 3);
    var e = searchInput.text.substr(-3);
    if (b.startsWith("/")) {
        var s = b.substr(1);
    }
    else if (e.startsWith("/")) {
        var s = e.substr(1);
    }
    else {
        return false;
    }

    if (s.length === 2) {
        for (var i=0; i<hoogleCommands.length; i++) {
            if (hoogleCommands[i] === s) {
                return true;
            }
        }
    }
    return false;
}
