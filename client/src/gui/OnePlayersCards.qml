import QtQuick 2.7

Column {
    anchors.verticalCenter: parent.verticalCenter
    anchors.verticalCenterOffset: -45

    anchors.leftMargin: (anchors.left == parent.left) ? 130 : undefined
    anchors.rightMargin: (anchors.right == parent.right) ? 130 : undefined

    spacing: 10
    
    Repeater {
        model: 2
        Card {
            rotation: 90
        }
    }
}
