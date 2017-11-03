import QtQuick.Controls 2.2
import QtQuick 2.7

ComboBox
{
    property alias dropDownText: dropDown.text

    enabled: guiEnabled

    delegate: ItemDelegate
    {
        id: item

        width: iterations.width

        contentItem: Text
        {
            text: modelData
            font.pointSize: 15
        }

        MouseArea
        {
            anchors.fill: parent

            hoverEnabled: true
            propagateComposedEvents: true

            onClicked: mouse.accepted = false
            onPressed: mouse.accepted = false
            onReleased: mouse.accepted = false
            onDoubleClicked: mouse.accepted = false
            onPositionChanged: mouse.accepted = false
            onPressAndHold: mouse.accepted = false

            onEntered:
            {
                item.highlighted = true
            }

            onExited:
            {
                item.highlighted = false
            }
        }
    }

    contentItem: Text
    {
        id: dropDown

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter

        font.pointSize: 15
        opacity: enabled ? 1.0: 0.3
    }
}
