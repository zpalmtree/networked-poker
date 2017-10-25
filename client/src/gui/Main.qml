import QtQuick.Window 2.2
import "Constants.js" as Constants

Window
{
    id: mainWindow

    title: "Poker Client"
    visible: true

    minimumHeight: Constants.initialHeight
    minimumWidth: Constants.initialWidth

    color: "black"

    Board {}

    TableCards {}

    Pot {}

    Buttons {id: buttons}

    AllChipsAndCards {}

    RaiseWindow {id: raiseWindow}

    GameOverWindow
    {
        id: lossWindow
        visible: lossWindowVisible

        text: "Game Over!\n\nUnfortunately you ran out of chips.\n" +
              "Thanks for playing!"
    }

    GameOverWindow
    {
        id: winnerWindow
        visible: winWindowVisible

        text: "Congratulations, you won!\n\nThanks for playing!"
    }

    Console {}
}    
