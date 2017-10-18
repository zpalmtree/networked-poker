import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout
{
    height: 80

    anchors.bottom: parent.bottom
    anchors.right: parent.right
    anchors.left: parent.left

    Action
    {
        enabled: bEnabled[0]
        Layout.leftMargin: 10
        text: "Fold"
        onClicked: fold
    }

    Action
    {
        enabled: bEnabled[1]
        text: "Check"
        onClicked: check
    }

    Action
    {
        enabled: bEnabled[2]
        text: "Call"
        onClicked: call
    }

    Action
    {
        enabled: bEnabled[3]
        text: "Raise"
        onClicked: raise
    }

    Action
    {
        enabled: bEnabled[4]
        Layout.rightMargin: 10
        text: "All In"
        onClicked: allIn
    }
}
