import QtQuick.Window 2.2
import QtQuick.Controls 2.2
import QtQuick 2.7
import QtQuick.Layouts 1.3

Window
{
    title: "Shuffle Tester"
    visible: true

    minimumHeight: 900
    minimumWidth: 1600

    GridLayout
    {
        id: grid

        columns: 4

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 10

        height: 80

        CustomLabel
        {
            text: "Shuffle Type"
            Layout.fillWidth: true
        }

        CustomLabel
        {
            text: "Source of Randomness"
            Layout.fillWidth: true
        }

        CustomLabel
        {
            text: "Iterations"
            Layout.fillWidth: true
        }

        CustomLabel
        {
            text: "Launch Test"
            Layout.fillWidth: true
        }

        CustomComboBox
        {
            id: drawAlgorithm

            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            model: ["RandomIndex", "Knuth", "RandomSort"]

            dropDownText: drawAlgorithm.displayText
        }

        CustomComboBox
        {
            id: rngSource

            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            model: ["LEucyer", "Mersenne", "MWC256"]

            dropDownText: rngSource.displayText
        }

        CustomComboBox
        {
            id: iterations

            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

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
                testShuffle(drawAlgorithm.currentText, rngSource.currentText, 
                            iterations.currentText)
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
