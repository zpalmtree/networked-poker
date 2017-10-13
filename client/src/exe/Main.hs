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

import Graphics.QML
    (initialDocument, contextObject, newObject, defaultEngineConfig, 
     fileDocument, anyObjRef, runEngineLoop)

import ClientTypes (CGameStateT)
import HandleMessage (handleMsg)
import Utilities (decode)
import GUISetup (initialSetup, makeClass)

import Paths_client (getDataFileName)

main :: IO ()
main = withSocketsDo $ do   
    gui <- getDataFileName "src/gui/Main.qml"
    
    (rootClass, sigs) <- makeClass

    ctx <- newObject rootClass ()

    trySetup <- initialSetup sigs

    case trySetup of
        (Left err) -> do
            putStrLn "Couldn't connect to server. Did you start it?"
            print err

        (Right (initialState, sock)) -> do

            forkIO $ evalStateT (ioLoop sock) initialState

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
            handleMsg msg'
            ioLoop sock
