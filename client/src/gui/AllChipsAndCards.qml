import QtQuick 2.7
import "Constants.js" as Constants

Item {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.verticalCenter: parent.verticalCenter

    anchors.verticalCenterOffset: (-0.5 * Constants.bottomOffset) + 
                                  (0.5 * Constants.topOffset)

    anchors.horizontalCenterOffset: (-0.5 * Constants.leftOffset) -
                                    (0.5 * Constants.rightOffset)

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

            inPlay: pInPlay[index]

            borderEnabled: pCurrentPlayer[index]
        }
    }
}
