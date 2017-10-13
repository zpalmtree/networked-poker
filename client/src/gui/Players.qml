import QtQuick 2.7

Item
{
    anchors.fill: parent

    property var playerNames: pNames

    TwoPlayers
    {
        anchors.bottom: parent.bottom

        Player
        {
            text: playerNames[1]
        }
        
        Player
        {
            text: playerNames[0]
        }
    }

    TwoPlayers
    {
        Player
        {
            text: playerNames[3]
        }

        Player
        {
            text: playerNames[4]
        }
    }

    OnePlayer
    {
        anchors.left: parent.left

        Player
        {
            text: playerNames[2]
        }
    }

    OnePlayer
    {
        anchors.right: parent.right

        Player
        {
            text: playerNames[5]
        }
    }
}
