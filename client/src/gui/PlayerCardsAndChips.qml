import QtQuick 2.7

Item
{
    anchors.fill: parent

    XCardsAndChips
    {
        cardArr1: p2Cards
        cardArr2: p1Cards

        chipVal1: p2Chips
        chipVal2: p1Chips
        
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 220
    }

    XCardsAndChips
    {
        cardArr1: p5Cards
        cardArr2: p4Cards

        chipVal1: p5Chips
        chipVal2: p4Chips
        
        rotate: true
        anchors.top: parent.top
        anchors.topMargin: 120
    }

    YCardsAndChips
    {
        cardArr: p3Cards
        chipVal: p3Chips

        anchors.left: parent.left
    }

    YCardsAndChips
    {
        cardArr: p6Cards
        chipVal: p6Chips

        anchors.right: parent.right
        rotate: true
    }
}
