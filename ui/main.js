.import "match.js" as MatchJs
.import "tab.js" as TabJs
.import "util.js" as UtilJs


// =====================================================================
// Model that controls the UI
// =====================================================================
var MODE = {
    inputQuery: 0,
    selecting: 1
}

function inMode(mode) {
    return window.currentMode === mode;
}

function enterMode(mode) {
  // console.log("mode transition: " + [window.currentMode, mode].toString());
  if (! inMode(mode)) {
    window.currentMode = mode;

    // do these if a transition happened
    if (mode === MODE.selecting) {
      matchContainer.selected = matchBoundIndex(0);
    }
    else if (mode === MODE.inputQuery) {
      matchContainer.selected = -1;
    } else {
        null.x = 1;
    }
  }
}


// =====================================================================
// Match manipulation
// =====================================================================

function matchBoundIndex(index) {
    return UtilJs.boundIndex(index, matchContainer.count);
}

function matchSelect(indexIn) {
    var index = matchBoundIndex(indexIn);
    if (index >= 0 && matchContainer.selected !== index) {
        enterMode(MODE.selecting);
        matchContainer.selected = index;
    }
}

// Select the specified match, and open it in a calculated tab.
function matchOpen(indexIn) {
    var index = matchBoundIndex(indexIn);
    if (!inMode(MODE.inputQuery) && index >= 0) {
        matchSelect(index);

        var tabIndex;
        var targetTab;

        // TODO @incomplete: make this number configurable
        if (tabContainer.count < 10) {
            tabIndex = tabContainer.count;
            var component = Qt.createComponent("Doc.qml");
            targetTab = tabContainer.insertTab(tabIndex, "Loading", component);
            targetTab.active = true;
            targetTab.item.url = matchContainer.model[index].url;
            targetTab.item.selectedName = matchContainer.model[index].name;
            targetTab.title = Qt.binding(function(){
                var newIndex = tabIndexOf(targetTab);
                var newTab = tabContainer.getTab(newIndex);
                return TabJs.title(newIndex, newTab.item.selectedName)
            });


            tabSelect(tabIndex);
        } else {
            tabIndex = tabContainer.currentIndex;
            targetTab = tabContainer.getTab(tabIndex);
            targetTab.active = true;

            targetTab.item.url = matchContainer.model[index].url;
            targetTab.item.selectedName = matchContainer.model[index].name;
            targetTab.title = Qt.binding(function(){
                var newIndex = tabIndexOf(targetTab);
                var newTab = tabContainer.getTab(newIndex);
                return TabJs.title(newIndex, newTab.item.selectedName)
            });
        }
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
    if (!inMode(MODE.inputQuery) && index >= 0 && tabContainer.currentIndex !== index) {
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
