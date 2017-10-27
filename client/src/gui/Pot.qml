import QtQuick 2.7
import "Constants.js" as Constants

Item
{
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter
    anchors.verticalCenterOffset: (-0.5 * Constants.bottomOffset) +
                                  (0.5 * Constants.topOffset)

    anchors.horizontalCenterOffset: -20 + (0.5 * Constants.leftOffset)
                                        - (0.5 * Constants.rightOffset)

    Chip
    {
        property var value: potValue
    }
}
