module Main
(
    main
)
where

import Control.Monad.Trans.State (evalStateT)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Control.Monad (void, when)

import System.Log.Logger
    (Priority(..), updateGlobalLogger, rootLoggerName, setLevel, infoM)

import Graphics.QML
    (initialDocument, contextObject, newObject, defaultEngineConfig, 
     fileDocument, anyObjRef, runEngineLoop)

import HandleMessage (handleMsg)
import ClientSetup (initialSetup, initialGUISetup, makeClass)
import ClientFramework (ioLoop)
import Utilities (getName)

import Paths_client (getDataFileName)

main :: IO ()
main = do 

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

        name <- getName

        trySetup <- initialSetup sigs ctx name

        case trySetup of
            (Left err) -> error $ 
                "Couldn't connect to server. Did you start it?\n" ++ show err

            (Right (initialState, sock)) -> do

                infoM "Prog.main" "Setting up GUI"

                evalStateT initialGUISetup initialState

                infoM "Prog.main" "Entering network loop"

                void . forkIO $ evalStateT (ioLoop sock handleMsg) initialState

    infoM "Prog.main" "Running GUI"

    let config = defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    runEngineLoop config
