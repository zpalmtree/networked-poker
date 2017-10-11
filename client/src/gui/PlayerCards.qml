import QtQuick 2.7

Item {
    anchors.fill: parent

    TwoPlayersCards {
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 220
    }

    TwoPlayersCards {
        rotate: true
        anchors.top: parent.top
        anchors.topMargin: 120
    }

    OnePlayersCards {
        anchors.left: parent.left
    }

    OnePlayersCards {
        anchors.right: parent.right
        rotate: true
    }
}
