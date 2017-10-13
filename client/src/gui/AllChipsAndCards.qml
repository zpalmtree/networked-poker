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

            card1Img: cardArr[index][0]
            card2Img: cardArr[index][1]

            chipValue: chipArr[index]

            playerName: nameArr[index]

            indexNum: index
        }
    }
}
