import QtQuick 2.7
import "Constants.js" as Constants

Rectangle
{
    color: "green"
    radius: 90

    anchors.fill: parent

    anchors.leftMargin: 105
    anchors.rightMargin: 105
    anchors.topMargin: 105
    anchors.bottomMargin: 205 + Constants.bottomOffset
}
