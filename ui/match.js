.pragma library

var keys = "ASDFWERTC";

function matchQ(index) {
    return Math.floor(index / keys.length);
}

function matchR(index) {
    return index % keys.length;
}

function shortcut(index) {
    var key = keys[matchR(index)];
    var q = matchQ(index);
    if (q === 0) {
        return key;
    } else if (q === 1) {
        return "G," + key;
    } else if (q === 2) {
        return "V," + key;
    }
}

function shortcutText(index) {
    return keys[matchR(index)];
}

function shortcutColor(index) {
    var q = matchQ(index);
    if (q === 0) {
        return "blue";
    } else if (q === 1) {
        return "green";
    } else if (q === 2) {
        return "blue";
    }
}



function icon(vendor, collection) {
    if (vendor === "DevDocs") {
        var l = collection.toLowerCase();
        l = l.replace(/-/g, '_');
        l = l.replace(/ /g, '_');
        if (l === "c++") l = "cpp";
        if (l === "nginx_lua_module") l = "nginx";
        if (l === "löve") l = "love";
        return "icon/" + l + "/16@2x.png";
    } else if (vendor === "Hoogle") {
        return "icon/" + "haskell" + "/16@2x.png";
    } else {
        return null;
    }
}
