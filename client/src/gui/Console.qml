import QtQuick 2.7
import QtQuick.Window 2.2
import QtQuick.Controls 2.2
import "Constants.js" as Constants

Item
{
    height: mainWindow.height
    width: Constants.rightOffset
    anchors.right: parent.right

    Rectangle
    {
        id: consoleLabel
        height: 30

        anchors.right: parent.right
        anchors.left: parent.left
        anchors.top: parent.top

        anchors.topMargin: 10
        anchors.rightMargin: 10

        color: "black"
        border.color: "#00FF00"

        Text
        {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            color: "#00FF00"
            text: "Console"
            font.pointSize: 12
        }
    }

    Rectangle
    {
        id: consoleText
        color: "black"
        border.color: "#00FF00"

        anchors.top: consoleLabel.bottom
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.left: parent.left

        anchors.bottomMargin: 10
        anchors.rightMargin: 10

        ListView
        {
            id: consoleLog
            ScrollBar.vertical: ScrollBar {}

            clip: true
            anchors.fill: parent
            anchors.margins: 5

            model: messages

            onCountChanged:
            {
                var newIndex = count - 1
                positionViewAtEnd()
                currentIndex = newIndex
            }

            delegate: Text
            {
                width: parent.width
                wrapMode: Text.Wrap
                color: "#00FF00"
                text: modelData
            }
        }
    }
}
