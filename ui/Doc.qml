import QtQuick 2.9
import QtWebEngine 1.5
import QtQuick.Controls 1.4

import co.aixon.docbrowser 1.0
WebEngineView {
    property string selectedName
    zoomFactor: Style.webEngineZoomFactor
}
