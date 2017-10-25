import QtQuick 2.7
import QtQuick.Controls 2.2
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import "Constants.js" as Constants

Rectangle
{
    property alias text: msg.text

    function getWindowXOffset()
    {
        return (Window.width - Constants.initialWidth) / 2
    }

    function getWindowYOffset()
    {
        return (Window.height - Constants.initialHeight) / 2
    }

    anchors.fill: parent

    anchors.leftMargin: 350 + getWindowXOffset()
    anchors.rightMargin: 350 + getWindowXOffset()
    anchors.topMargin: 260 + getWindowYOffset() - (msg.contentHeight / 2)
    anchors.bottomMargin: 360 + Constants.bottomOffset + getWindowYOffset() 
                              - (msg.contentHeight / 2)

    radius: 10
    border.width: 3
    border.color: "black"
    color: "white"

    ColumnLayout
    {
        spacing: 10

        anchors.fill: parent

        Text
        {
            id: msg

            Layout.margins: 10

            anchors.left: parent.left
            anchors.right: parent.right
            horizontalAlignment: Text.AlignHCenter
            font.pointSize: 12
        }

        Button
        {
            Layout.rightMargin: 20
            Layout.leftMargin: 20
            Layout.bottomMargin: 10

            Layout.alignment: Qt.AlignCenter
            Layout.fillWidth: true
            Layout.fillHeight: true

            text: "Quit"

            onClicked: mainWindow.close()
        }
    }
}
