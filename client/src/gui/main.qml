import QtQuick.Window 2.2
import QtQuick 2.7
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls.Styles 1.4

Window {
    title: "Poker Client"
    visible: true

    minimumHeight: 600
    minimumWidth: 700

    color: "black"

    Rectangle {
        color: "green"
        border.color: "black"
        border.width: 5

        radius: 30
        antialiasing: true

        anchors.fill: parent

        anchors.leftMargin: 100
        anchors.rightMargin: 100
        anchors.topMargin: 100
        anchors.bottomMargin: 200
    }

    Row {
        anchors.horizontalCenter: parent.horizontalCenter

        spacing: 100

        topPadding: 18

        Player {
            text: "Dave"
        }

        Player {
            text: "Bob"
        }
    }

    Row {
        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter

        spacing: 100

        bottomPadding: 120

        Player {
            text: "Jim"
        }

        Player {
            text: "Pete"
        }
    }

    Column {
        anchors.left: parent.left
        anchors.verticalCenter: parent.verticalCenter

        leftPadding: 20
        bottomPadding: 95

        Player {
            text: "Steve"
        }
    }

    Column {
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter

        rightPadding: 20
        bottomPadding: 95

        Player {
            text: "Gary"
        }
    }

    RowLayout {
        height: 100

        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.left: parent.left

        Action {
            Layout.leftMargin: 10
            text: "Fold"
        }

        Action {
            text: "Check"
        }

        Action {
            text: "Call"
        }

        Action {
            text: "Raise"
        }

        Action {
            Layout.rightMargin: 10
            text: "All In"
        }
    }
}    
