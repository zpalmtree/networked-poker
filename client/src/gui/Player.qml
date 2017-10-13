import QtQuick 2.7

Rectangle
{
    property alias text: label.text

    height: 70
    width: 70

    radius: 10

    Text
    {
        id: label
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
    }
}
