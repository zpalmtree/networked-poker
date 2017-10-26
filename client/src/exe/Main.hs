module Main
(
    main
)
where

import Network.Socket.ByteString (recv, send)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (forkIO)
import Network.Socket (Socket, withSocketsDo)
import System.Environment (getArgs)
import Data.Binary (encode, decodeOrFail)
import Control.Monad (void, when)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict, null, fromStrict)

import System.Log.Logger
    (Priority(..), updateGlobalLogger, rootLoggerName, setLevel, infoM)

import Graphics.QML
    (initialDocument, contextObject, newObject, defaultEngineConfig, 
     fileDocument, anyObjRef, runEngineLoop)

import ClientTypes (CGameStateT)
import HandleMessage (handleMsg)
import Setup (initialSetup, initialGUISetup, makeClass)

import Paths_client (getDataFileName)

main :: IO ()
main = withSocketsDo $ do 

    args <- getArgs

    let level | "--debug" `elem` args = DEBUG
              | "--info" `elem` args = INFO
              | otherwise = WARNING

    let networkEnabled | "--guionly" `elem` args = False
                       | otherwise = True

    updateGlobalLogger rootLoggerName (setLevel level)

    gui <- getDataFileName "src/gui/Main.qml"
    
    (rootClass, sigs) <- makeClass

    ctx <- newObject rootClass ()

    when networkEnabled $ do

        infoM "Prog.main" "Getting initial state"

        trySetup <- initialSetup sigs ctx

        case trySetup of
            (Left err) -> error $ 
                "Couldn't connect to server. Did you start it?\n" ++ show err

            (Right (initialState, sock)) -> do

                infoM "Prog.main" "Setting up GUI"

                evalStateT initialGUISetup initialState

                infoM "Prog.main" "Entering network loop"

                void . forkIO $ evalStateT (ioLoop sock) initialState

    infoM "Prog.main" "Running GUI"

    let config = defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    runEngineLoop config

ioLoop :: Socket -> CGameStateT ()
ioLoop sock = do
    msg <- lift $ recv sock 4096
    decode msg sock

decode :: BS.ByteString -> Socket -> CGameStateT ()
decode input sock = case decodeOrFail $ BL.fromStrict input of
    Left (_, _, err) -> error err
    Right (unconsumed, _, msg) -> do
        maybeAction <- handleMsg msg
        case maybeAction of
            Nothing -> return ()
            Just action -> do
                lift . infoM "Prog.decode" $
                    "Sending message to server: " ++ show action

                let actionMsg = BL.toStrict $ encode action
                
                void . lift $ send sock actionMsg

        if BL.null unconsumed
            then ioLoop sock
            else decode (BL.toStrict unconsumed) sock
