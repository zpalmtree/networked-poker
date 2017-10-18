{-# LANGUAGE TypeFamilies #-}
module HandleClick
(
    handleFold,
    handleCheck,
    handleCall,
    handleRaise,
    handleAllIn
)
where

import Graphics.QML (ObjRef, MarshalMode, IIsObjType, Yes, ICanPassTo, Marshal)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Lens ((^.))
import Control.Monad (unless)
import System.Log.Logger (infoM)

import ClientTypes (StatesNSignals)
import CLenses (actionMade)
import Types (Action(..))

handleAction :: StatesNSignals -> Action Int -> String -> IO ()
handleAction sNs action name = do
    putStrLn "CLICK!"
    infoM "Prog.handleAction" $ "User chose to " ++ show action

    success <- tryPutMVar (sNs^.actionMade) action
    unless success .
        error $ "Couldn't put MVar in " ++ name ++ "!"

--handleFold :: StatesNSignals -> ObjRef () -> IO ()
handleFold :: (MarshalMode tt IIsObjType () ~ Yes,
               MarshalMode tt ICanPassTo () ~ Yes,
               Marshal tt) => StatesNSignals -> tt -> IO ()
handleFold sNs _ = handleAction sNs Fold "handleFold"

--handleCheck :: StatesNSignals -> ObjRef () -> IO ()

handleCheck :: (MarshalMode tt IIsObjType () ~ Yes,
               MarshalMode tt ICanPassTo () ~ Yes,
               Marshal tt) => StatesNSignals -> tt -> IO ()
handleCheck sNs _ = handleAction sNs Check "handleCheck"

--handleCall :: StatesNSignals -> ObjRef () -> IO ()
handleCall :: (MarshalMode tt IIsObjType () ~ Yes,
               MarshalMode tt ICanPassTo () ~ Yes,
               Marshal tt) => StatesNSignals -> tt -> IO ()

handleCall sNs _ = handleAction sNs Call "handleCall"

--need to make some gui element appear here with valid raise values
--handleRaise :: StatesNSignals -> ObjRef () -> IO ()
handleRaise :: (MarshalMode tt IIsObjType () ~ Yes,
               MarshalMode tt ICanPassTo () ~ Yes,
               Marshal tt) => StatesNSignals -> tt -> IO ()

handleRaise = undefined

--handleAllIn :: StatesNSignals -> ObjRef () -> IO ()
handleAllIn :: (MarshalMode tt IIsObjType () ~ Yes,
               MarshalMode tt ICanPassTo () ~ Yes,
               Marshal tt) => StatesNSignals -> tt -> IO ()

handleAllIn sNs _ = handleAction sNs AllIn "handleAllIn"
