import QtQuick 2.7

Item
{
    width: 40
    height: 40

    Image
    {
        source: "assets/chip.png"
        width: parent.width
        height: parent.height
    }

    Text
    {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        text: value
        color: "white"
        style: Text.Outline
        styleColor: "black"
    }
}
