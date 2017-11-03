import QtQuick.Window 2.2
import QtQuick.Controls 1.4
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

        ComboBox
        {
            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            id: shuffleType

            property string value: shuffleType.currentText

            model: ["RandomIndex", "KnuthShuffle"]

            style: ComboBoxStyle
            {
                label: Text
                {
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    font.pointSize: 15
                    text: control.currentText
                }

                property Component __dropDownStyle: MenuStyle
                {
                    itemDelegate.label: Text
                    {
                        verticalAlignment: Text.AlignVCenter
                        horizontalAlignment: Text.AlignHCenter
                        font.pointSize: 15
                        text: styleData.text
                    }
                }
            }
        }

        ComboBox
        {
            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            id: iterations

            property int value: Number(iterations.currentText)

            currentIndex: 2

            model: [1, 10, 100, 1000, 10000, 100000]

            style: ComboBoxStyle
            {
                label: Text
                {
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    font.pointSize: 15
                    text: control.currentText
                }

                property Component __dropDownStyle: MenuStyle
                {
                    itemDelegate.label: Text
                    {
                        verticalAlignment: Text.AlignVCenter
                        horizontalAlignment: Text.AlignHCenter
                        font.pointSize: 15
                        text: styleData.text
                    }
                }
            }
        }
        
        Button
        {
            enabled: guiEnabled

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumWidth: 150

            text: "Go"

            style: ButtonStyle
            {
                label: Text
                {
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    font.pointSize: 15
                    text: control.text
                }
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
