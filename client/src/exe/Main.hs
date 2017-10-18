module Main
(
    main
)
where

import Network.Socket.ByteString (recv)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (forkIO)
import Network.Socket (Socket, withSocketsDo)
import System.Environment (getArgs)

import System.Log.Logger
    (Priority(..), updateGlobalLogger, rootLoggerName, setLevel, infoM)

import Graphics.QML
    (initialDocument, contextObject, newObject, defaultEngineConfig, 
     fileDocument, anyObjRef, runEngineLoop)

import ClientTypes (CGameStateT)
import HandleMessage (handleMsg)
import Utilities (decode)
import Setup (initialSetup, initialGUISetup, makeClass)

import Paths_client (getDataFileName)

main :: IO ()
main = withSocketsDo $ do 

    args <- getArgs

    let level | "--debug" `elem` args = DEBUG
              | "--info" `elem` args = INFO
              | otherwise = WARNING

    updateGlobalLogger rootLoggerName (setLevel level)

    gui <- getDataFileName "src/gui/Main.qml"
    
    (rootClass, sigs) <- makeClass

    ctx <- newObject rootClass ()

    infoM "Prog.main" "Getting initial state"

    trySetup <- initialSetup sigs ctx

    case trySetup of
        (Left err) -> error $ 
            "Couldn't connect to server. Did you start it?" ++ show err

        (Right (initialState, sock)) -> do

            infoM "Prog.main" "Setting up GUI"

            evalStateT initialGUISetup initialState

            infoM "Prog.main" "Entering network loop"

            forkIO $ evalStateT (ioLoop sock) initialState

            infoM "Prog.main" "Running GUI"

            runEngineLoop defaultEngineConfig {
                initialDocument = fileDocument gui,
                contextObject = Just $ anyObjRef ctx
            }

ioLoop :: Socket -> CGameStateT ()
ioLoop sock = do
    msg <- lift $ decode <$> recv sock 4096
    case msg of
        Left (_, _, err) -> error err
        Right (_, _, msg') -> do
            maybeMsg <- handleMsg msg'

            case maybeMsg of
                Nothing -> return ()
                Just msg'' -> do
                    lift $ infoM "Prog.main" "Sending message to server"
                    --send blah blah
                    return ()

            ioLoop sock
