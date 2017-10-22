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

    Buttons {}

    AllChipsAndCards {}

    RaiseWindow {}
}    
