import QtQuick.Window 2.2
import QtQuick 2.7

Window {
    title: "Poker Client"
    visible: true

    minimumHeight: 400
    minimumWidth: 600

    color: "black"

    Rectangle {
        color: "green"
        border.color: "black"
        border.width: 5

        radius: 30
        antialiasing: true

        anchors.fill: parent

        anchors.leftMargin: 20
        anchors.rightMargin: 20
        anchors.topMargin: 20
        anchors.bottomMargin: 20
    }
}    
