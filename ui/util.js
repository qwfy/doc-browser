.pragma library

function boundIndex(index, containerSize) {
    if (containerSize <= 0) {
        return -1;
    } else {
        var largest = containerSize - 1;
        if (index > largest) {
            return 0;
        } else if (index < 0) {
            return largest;
        } else {
            return index;
        }
    }
}
