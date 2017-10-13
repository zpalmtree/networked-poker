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
    }

    Action
    {
        enabled: bEnabled[1]
        text: "Check"
    }

    Action
    {
        enabled: bEnabled[2]
        text: "Call"
    }

    Action
    {
        enabled: bEnabled[3]
        text: "Raise"
    }

    Action
    {
        enabled: bEnabled[4]
        Layout.rightMargin: 10
        text: "All In"
    }
}
