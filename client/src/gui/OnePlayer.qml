import QtQuick 2.7

Column {
    anchors.verticalCenter: parent.verticalCenter

    leftPadding: (anchors.left == parent.left) ? 20 : undefined
    rightPadding: (anchors.right == parent.right) ? 20 : undefined

    bottomPadding: 95
}
