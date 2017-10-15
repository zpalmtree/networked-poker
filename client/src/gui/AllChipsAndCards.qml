import QtQuick 2.7

Item {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    Repeater
    {
        model: 6
        CardsChipsPlayer
        {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter

            visible: pVisible[index]

            card1Img: pCards[index][0]
            card2Img: pCards[index][1]

            betValue: pBets[index]

            playerName: pNames[index]

            indexNum: index
        }
    }
}
