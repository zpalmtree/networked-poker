import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout {
    height: 80

    anchors.bottom: parent.bottom
    anchors.right: parent.right
    anchors.left: parent.left

    Action {
        enabled: foldEnabled
        Layout.leftMargin: 10
        text: "Fold"
    }

    Action {
        enabled: checkEnabled
        text: "Check"
    }

    Action {
        enabled: callEnabled
        text: "Call"
    }

    Action {
        enabled: raiseEnabled
        text: "Raise"
    }

    Action {
        enabled: allInEnabled
        Layout.rightMargin: 10
        text: "All In"
    }
}
