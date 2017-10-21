import QtQuick 2.7

Rectangle
{
    property alias source: image.source

    color: "white"
    height: 80
    width: 60

    radius: 5

    rotation: parent.rotation

    Image
    {
        id: image
        width: parent.width
        height: parent.height
    }
}
