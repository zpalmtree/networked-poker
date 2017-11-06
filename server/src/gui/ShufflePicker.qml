import QtQuick.Window 2.2
import QtQuick.Controls 2.2
import QtQuick 2.7
import QtQuick.Layouts 1.3

Window
{
    title: "Shuffle Picker"
    visible: true

    minimumHeight: 250
    minimumWidth: 600

    GridLayout
    {
        id: grid

        columns: 3

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
            text: "Update Shuffle Type"
            Layout.fillWidth: true
        }

        CustomComboBox
        {
            id: drawAlgorithm

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            model: ["RandomIndex", "Knuth"]

            dropDownText: drawAlgorithm.displayText
        }

        CustomComboBox
        {
            id: rngSource

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            model: ["LEucyer", "Mersenne", "MWC256"]

            dropDownText: rngSource.displayText
        }

        Button
        {
            id: goButton

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
            }

            onClicked: 
            {
                changeShuffle(drawAlgorithm.currentText, rngSource.currentText)
            }
        }
    }
}
