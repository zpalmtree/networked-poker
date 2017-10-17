import QtQuick 2.7

Rectangle
{
    property alias text: label.text
    property alias textColor: label.color

    height: 70
    width: 70

    radius: 10

    border.color: "yellow"

    Text
    {
        id: label
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        horizontalAlignment: Text.AlignHCenter
    }
}
