import QtQuick.Window 2.2
import QtQuick.Controls 1.4
import QtQuick 2.7
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls.Private 1.0

Window
{
    title: "Shuffle Picker"
    visible: true

    minimumHeight: 50
    minimumWidth: 300

    ComboBox
    {
        model: ["RandomIndex", "KnuthShuffle"]
        anchors.fill: parent

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

        onActivated:
        {
            changeShuffle(currentText)
        }
    }
}
