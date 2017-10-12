import QtQuick 2.7

Item {
    anchors.fill: parent

    XCardsAndChips {
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 220
    }

    XCardsAndChips {
        rotate: true
        anchors.top: parent.top
        anchors.topMargin: 120
    }

    YCardsAndChips {
        anchors.left: parent.left
    }

    YCardsAndChips {
        anchors.right: parent.right
        rotate: true
    }
}
