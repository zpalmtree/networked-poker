import QtQuick 2.7
import "Constants.js" as Constants

Item
{
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter
    anchors.verticalCenterOffset: -0.5 * Constants.bottomOffset

    anchors.horizontalCenterOffset: -20

    Chip
    {
        property var value: potValue
    }
}
