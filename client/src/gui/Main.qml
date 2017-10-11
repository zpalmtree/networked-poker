import QtQuick.Window 2.2
import QtQuick 2.7
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls.Styles 1.4

Window {
    id: mainWindow

    title: "Poker Client"
    visible: true

    minimumHeight: 700
    minimumWidth: 1000

    color: "black"

    Board {}

    Players {}

    PlayerCards {}

    TableCards {}

    Buttons {}
}    
