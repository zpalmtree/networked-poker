import QtQuick 2.7
import QtQuick.Layouts 1.3

ColumnLayout {
    property var rotate

    rotation: rotate ? 180 : 0

    anchors.verticalCenter: parent.verticalCenter
    anchors.verticalCenterOffset: rotate ? -70 : -25

    anchors.leftMargin: (anchors.left == parent.left) ? 130 : undefined
    anchors.rightMargin: (anchors.right == parent.right) ? 130 : undefined

    spacing: 10

    Repeater {
        model: 2
        Card {
            rotation: 90
        }
    }

    Chip {
        Layout.alignment: Qt.AlignCenter
    }
}
