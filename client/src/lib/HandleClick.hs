module HandleClick
(
    handleFold,
    handleCheck,
    handleCall,
    handleRaise,
    handleAllIn
)
where

import Graphics.QML (ObjRef, fireSignal)
import Control.Concurrent.MVar (tryPutMVar)
import Data.IORef (writeIORef)
import Control.Lens ((^.))
import Control.Monad (unless)
import System.Log.Logger (infoM)

import ClientTypes (StatesNSignals)
import CLenses (actionMade, bEnabledS, bEnabledSig)
import Types (Action(..))
import Constants (numButtons)

pushAction :: Action Int -> String -> StatesNSignals -> ObjRef () -> IO ()
pushAction action name sNs this = do
    infoM "Prog.pushAction" $ "User chose to " ++ show action

    success <- tryPutMVar (sNs^.actionMade) action
    unless success .
        error $ "Couldn't put MVar in " ++ name ++ "!"

    let bools = replicate numButtons False

    writeIORef (sNs^.bEnabledS) bools
    fireSignal (sNs^.bEnabledSig) this

handleFold :: StatesNSignals -> ObjRef () -> IO ()
handleFold = pushAction Fold "handleFold"

handleCheck :: StatesNSignals -> ObjRef () -> IO ()
handleCheck = pushAction Check "handleCheck"

handleCall :: StatesNSignals -> ObjRef () -> IO ()
handleCall = pushAction Call "handleCall" 

--need to make some gui element appear here with valid raise values
handleRaise :: StatesNSignals -> ObjRef () -> Int -> IO ()
handleRaise sNs this n = pushAction (Raise n) "handleRaise" sNs this

handleAllIn :: StatesNSignals -> ObjRef () -> IO ()
handleAllIn = pushAction AllIn "handleAllIn"
