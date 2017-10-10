import QtQuick 2.7

Item {
    anchors.fill: parent

    TwoPlayers {
        anchors.bottom: parent.bottom

        Player {
            text: "Dave"
        }
        
        Player {
            text: "Bob"
        }
    }

    TwoPlayers {
        Player {
            text: "Jim"
        }

        Player {
            text: "Pete"
        }
    }

    OnePlayer {
        anchors.left: parent.left

        Player {
            text: "Steve"
        }
    }

    OnePlayer {
        anchors.right: parent.right

        Player {
            text: "Gary"
        }
    }
}
