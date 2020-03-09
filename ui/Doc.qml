import QtQuick 2.13
import QtWebEngine 1.9
import QtQuick.Controls 2.13

import co.aixon.docbrowser 1.0
WebEngineView {
    property string selectedName
    zoomFactor: Style.webEngineZoomFactor
    settings.showScrollBars: true
}
