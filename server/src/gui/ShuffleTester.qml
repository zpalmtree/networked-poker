import QtQuick.Window 2.2
import QtQuick.Controls 2.2
import QtQuick 2.7
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls.Private 1.0
import QtQuick.Layouts 1.3

Window
{
    title: "Shuffle Tester"
    visible: true

    minimumHeight: 700
    minimumWidth: 800

    GridLayout
    {
        id: grid

        columns: 3

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 10

        height: 80

        Label
        {
            font.pointSize: 12
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            Layout.fillWidth: true
            text: "Shuffle Type"
        }

        Label
        {
            font.pointSize: 12
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            Layout.fillWidth: true
            text: "Iterations"
        }

        Label
        {
            font.pointSize: 12
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            Layout.fillWidth: true
            text: "Launch Test"
        }

        CustomComboBox
        {
            id: shuffleType

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            property string value: shuffleType.currentText

            model: ["RandomIndex", "KnuthShuffle"]

            dropDownText: shuffleType.displayText
        }

        CustomComboBox
        {
            id: iterations

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            property int value: Number(iterations.currentText)

            currentIndex: 2

            model: [1, 10, 100, 1000, 10000, 100000]

            dropDownText: iterations.displayText
        }

        Button
        {
            id: goButton

            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            text: "Go"

            contentItem: Text
            {
                text: goButton.text

                verticalAlignment: Text.AlignVCenter
                horizontalAlignment: Text.AlignHCenter

                font.pointSize: 15

                opacity: enabled ? 1.0: 0.3
            }

            onClicked: 
            {
                testShuffle(shuffleType.value, iterations.value)
            }
        }
    }

    Image
    {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.top: grid.bottom

        fillMode: Image.PreserveAspectFit
        source: chartLocation
    }
}
