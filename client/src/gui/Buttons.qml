import QtQuick 2.7
import QtQuick.Layouts 1.3
import "Constants.js" as Constants

RowLayout
{
    id: buttons

    height: 80

    anchors.bottom: parent.bottom
    anchors.right: parent.right
    anchors.left: parent.left

    anchors.bottomMargin: Constants.bottomOffset
    anchors.rightMargin: Constants.rightOffset
    anchors.leftMargin: Constants.leftOffset

    property var allEnabled: true

    Action
    {
        enabled: bEnabled[0] && allEnabled
        Layout.leftMargin: 10
        text: "Fold"
        onClicked: fold()
    }

    Action
    {
        enabled: bEnabled[1] && allEnabled
        text: "Check"
        onClicked: check()
    }

    Action
    {
        enabled: bEnabled[2] && allEnabled
        text: "Call"
        onClicked: call()
    }

    Action
    {
        enabled: bEnabled[3] && allEnabled
        text: "Raise"
        onClicked: allEnabled = false, raiseWindow.visible = true
    }

    Action
    {
        enabled: bEnabled[4] && allEnabled
        Layout.rightMargin: 10
        text: "All In"
        onClicked: allIn()
    }
}
