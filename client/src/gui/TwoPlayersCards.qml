import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout {
    anchors.horizontalCenter: parent.horizontalCenter

    Card {
        Layout.rightMargin: 5
    }

    Card{
        Layout.leftMargin: 5
        Layout.rightMargin: 50
    }

    Card{
        Layout.leftMargin: 50
        Layout.rightMargin: 5
    }

    Card{
        Layout.leftMargin: 5
    }
}
