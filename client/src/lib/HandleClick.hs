module HandleClick
(
    handleFold,
    handleCheck,
    handleCall,
    handleRaise,
    handleAllIn
)
where

import Graphics.QML (ObjRef)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Lens ((^.))
import Control.Monad (unless)

import ClientTypes (StatesNSignals)
import CLenses (actionMade)
import Types (Action(..))

handleAction :: StatesNSignals -> Action Int -> String -> IO ()
handleAction sNs action name = do
    success <- tryPutMVar (sNs^.actionMade) action
    unless success $
        error $ "Couldn't put MVar in " ++ name ++ "!"

handleFold :: StatesNSignals -> ObjRef () -> IO ()
handleFold sNs _ = handleAction sNs Fold "handleFold"

handleCheck :: StatesNSignals -> ObjRef () -> IO ()
handleCheck sNs _ = handleAction sNs Check "handleCheck"

handleCall :: StatesNSignals -> ObjRef () -> IO ()
handleCall sNs _ = handleAction sNs Call "handleCall"

--need to make some gui element appear here with valid raise values
handleRaise :: StatesNSignals -> ObjRef () -> IO ()
handleRaise = undefined

handleAllIn :: StatesNSignals -> ObjRef () -> IO ()
handleAllIn sNs _ = handleAction sNs AllIn "handleAllIn"
