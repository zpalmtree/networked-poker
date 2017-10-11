import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout {
    height: 80

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
