import QtQuick 2.7
import "Constants.js" as Constants

Row
{
    property var tableCardsArray: tCards

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    bottomPadding: 120 + Constants.bottomOffset

    spacing: 10

    Repeater
    {
        model: 5
        Card
        {
            source: tableCardsArray[index]
        }
    }
}
