.pragma library

var keys = "1234567890";

// TODO @incomplete: display title in reverse order: e.g. abspath.path.os
// so when there are many tabs, the most relavent part is still visible
function title(index, title) {
    return keys[index] + " " + title;
}
