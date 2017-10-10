import QtQuick 2.7

Row {
    anchors.horizontalCenter: parent.horizontalCenter

    spacing: 100

    topPadding: (anchors.bottom == parent.bottom) ? undefined : 18

    bottomPadding: (anchors.bottom == parent.bottom) ? 120 : undefined
}
