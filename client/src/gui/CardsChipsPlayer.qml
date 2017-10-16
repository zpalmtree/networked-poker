import QtQuick 2.7
import QtQuick.Window 2.2

Item
{
    property var betValue
    property var playerName
    property var card1Img
    property var card2Img
    property var indexNum
    property var inPlay

    function getRotation()
    {
        if (indexNum == 2)
        {
            return 90
        }
        else if (indexNum == 3 || indexNum == 4)
        {
            return 180
        }
        else if (indexNum == 5)
        {
            return 270
        }
        return 0
    }

    function getVerticalOffset()
    {
        if (indexNum == 0 || indexNum == 1)
        {
            return 50
        }
        else if (indexNum == 2)
        {
            return -120
        }
        else if (indexNum == 5)
        {
            return 20
        }
        return -150
    }

    function getHorizontalOffset()
    {
        if (indexNum == 0)
        {
            return 110
        }
        else if (indexNum == 1)
        {
            return -240
        }
        else if (indexNum == 2)
        {
            return -300
        }
        else if (indexNum == 3)
        {
            return -110
        }
        else if (indexNum == 4)
        {
            return 240
        }
        return 300
    }

    function getChipRotation()
    {
        if (indexNum == 2)
        {
            return 270
        }
        else if (indexNum == 3 || indexNum == 4)
        {
            return 180
        }
        else if (indexNum == 5)
        {
            return 90
        }
        return 0
    }

    function getCardRotation()
    {
        if (indexNum == 3 || indexNum == 4)
        {
            return 180
        }
        return 0
    }

    function getNameRotation()
    {
        return getChipRotation()
    }

    function getWindowXOffset()
    {
        var diff = (Window.width - 1000) / 2

        if (indexNum == 1 || indexNum == 2 || indexNum == 3)
        {
            diff *= -1
        }
        return diff
    }

    function getWindowYOffset()
    {
        var diff = (Window.height - 700) / 2

        if (indexNum == 0 || indexNum == 1)
        {
            return diff 
        }
        else if (indexNum == 3 || indexNum == 4)
        {
            return -diff
        }
        return 0
    }

    rotation: getRotation()

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    anchors.horizontalCenterOffset: getHorizontalOffset() + getWindowXOffset()
    anchors.verticalCenterOffset: getVerticalOffset() + getWindowYOffset()

    Column
    {
        spacing: 30
        
        Row
        {
            spacing: 10

            Card
            {
                rotation: getCardRotation()
                source: card1Img
            }

            Card
            {
                rotation: getCardRotation()
                source: card2Img
            }

            Chip
            {
                anchors.verticalCenter: parent.verticalCenter
                property int value: betValue
                rotation: getChipRotation()
            }
        }

        Player
        {
            color: inPlay ? "black" : "grey"
            rotation: getNameRotation()
            anchors.left: parent.left
            anchors.leftMargin: 30
            text: playerName
        }
    }
}
