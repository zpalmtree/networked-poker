import QtQuick 2.7
import QtQuick.Controls 2.2
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import "Constants.js" as Constants

Rectangle
{
    id: raiseWindow

    visible: false

    function getWindowXOffset()
    {
        return (Window.width - Constants.initialWidth) / 2
    }

    function getWindowYOffset()
    {
        return (Window.height - Constants.initialHeight) / 2
    }

    anchors.fill: parent

    anchors.leftMargin: 350 + getWindowXOffset()
    anchors.rightMargin: 350 + getWindowXOffset()
    anchors.topMargin: 200 + getWindowYOffset()
    anchors.bottomMargin: 300 + Constants.bottomOffset + getWindowYOffset()

    radius: 10
    border.width: 3
    border.color: "black"
    color: "white"

    Text
    {
        anchors.top: parent.top
        anchors.topMargin: 15
        anchors.horizontalCenter: parent.horizontalCenter

        font.pointSize: 12

        text: "What do you want to raise to?"
    }

    Slider
    {
        id: slide
        snapMode: Slider.SnapAlways
        from: slideMin
        to: slideMax
        value: slideMin
        stepSize: 1
        anchors.centerIn: parent
    }

    Text
    {
        // have to make it an int - the slider occasionaly gets real values
        // because of floating point math i guess, so we just cast it to an int
        id: slideValue
        property int intSliderValue: slide.value
        text: intSliderValue
        anchors.centerIn: parent
        anchors.verticalCenterOffset: -30
        font.pointSize: 12
    }

    Text
    {
        text: slide.from
        anchors.top: slide.bottom
        anchors.left: slide.left
        font.pointSize: 10
    }

    Text
    {
        text: slide.to
        anchors.top: slide.bottom
        anchors.right: slide.right
        font.pointSize: 10
    }

    RowLayout 
    {
        height: 40
        width: parent.width

        anchors.bottom: parent.bottom
        anchors.bottomMargin: 10
    
        anchors.right: parent.right
        anchors.rightMargin: 10

        anchors.left: parent.left
        anchors.leftMargin: 10

        spacing: 10

        Button
        {
            Layout.fillWidth: true
            Layout.fillHeight: true
            text: "OK"
            onClicked: raiseWindow.visible = false, 
                       raiseN(slideValue.intSliderValue),
                       buttons.allEnabled = true
        }

        Button
        {
            Layout.fillWidth: true
            Layout.fillHeight: true
            text: "Cancel"
            onClicked: raiseWindow.visible = false,
                       buttons.allEnabled = true
        }
    }
}
