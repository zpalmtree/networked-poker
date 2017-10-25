import QtQml.Models 2.2

ListModel
{
    function appendItems(items)
    {
        for (var i = 0; i < items.length; i++)
        {
            messageQueue.append({"msg": items[i]})
        }
    }

    id: messageQueue
    property var msg: logMsg
    onMsgChanged: appendItems(msg)
}
