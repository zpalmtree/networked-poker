import QtQuick 2.7

Item {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    property var cardArr: pCards
    property var chipArr: pChips
    property var nameArr: pNames

    Repeater
    {
        model: 6
        CardsChipsPlayer
        {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter

            card1Img: pCards[index][0]
            card2Img: pCards[index][1]

            chipValue: pChips[index]

            playerName: pNames[index]

            indexNum: index
        }
    }
}
