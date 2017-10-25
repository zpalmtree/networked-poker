import QtQuick 2.7
import "Constants.js" as Constants

Rectangle
{
    color: "green"
    radius: 90

    anchors.fill: parent

    anchors.leftMargin: 105 + Constants.leftOffset
    anchors.rightMargin: 105 + Constants.rightOffset
    anchors.topMargin: 105 + Constants.topOffset
    anchors.bottomMargin: 205 + Constants.bottomOffset
}
