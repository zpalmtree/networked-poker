import QtQuick 2.7
import QtQuick.Layouts 1.3

RowLayout
{
    property var cardArr1
    property var cardArr2

    property var chipVal1
    property var chipVal2

    property var rotate

    rotation: rotate ? 180 : 0

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.horizontalCenterOffset: rotate ? -25 : 25

    spacing: 10

    Repeater
    {
        model: 2

        Card
        {
            source: cardArr1[index]
        }
    }

    Chip
    {
        Layout.rightMargin: 130
        property int value: chipVal1
    }

    Repeater
    {
        model: 2
        
        Card
        {
            source: cardArr2[index]
        }
    }

    Chip
    {
        property int value: chipVal2
    }
}
