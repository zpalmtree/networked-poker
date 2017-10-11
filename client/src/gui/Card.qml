import QtQuick 2.7

Rectangle {
    color: "white"
    height: 80
    width: 60

    radius: 5

    rotation: parent.rotation

    Image {
        source: "assets/card-ace-spades.png"
        width: parent.width
        height: parent.height
    }
}
