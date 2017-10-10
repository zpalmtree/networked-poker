import QtQuick 2.7

Row {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    bottomPadding: 95

    spacing: 10

    Repeater {
        model: 5
        Card {}
    }
}
