import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout {
    property var rotate

    rotation: rotate ? 180 : 0

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.horizontalCenterOffset: rotate ? -25 : 25

    spacing: 10

    Card {}
    Card {}
    Chip {
        Layout.rightMargin: 130
    }
    Card {}
    Card {}
    Chip {}
}
