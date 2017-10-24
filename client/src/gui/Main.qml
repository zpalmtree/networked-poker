import QtQuick.Window 2.2

Window
{
    id: mainWindow

    title: "Poker Client"
    visible: true

    minimumHeight: 700
    minimumWidth: 1000

    color: "black"

    Board {}

    TableCards {}

    Pot {}

    Buttons {id: buttons}

    AllChipsAndCards {}

    RaiseWindow {id: raiseWindow}

    GameOverWindow {
        id: lossWindow
        visible: lossWindowVisible

        text: "Game Over!\n\nUnfortunately you ran out of chips.\n" +
              "Thanks for playing!"
    }

    GameOverWindow {
        id: winnerWindow
        visible: winWindowVisible

        text: "Congratulations, you won!\n\nThanks for playing!"
    }
}    
