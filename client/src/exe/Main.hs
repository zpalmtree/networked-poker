module Main
(
    main
)
where

import Data.Binary (Binary, encode, decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict, fromStrict)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Control.Concurrent (forkIO)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Graphics.QML
    (SignalKey, initialDocument, contextObject, newClass, newObject,
     defaultEngineConfig, fileDocument, anyObjRef, newSignalKey,
     defPropertySigRO', runEngineLoop)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrProtocol, addrSocketType, 
     addrAddress, connect)

import Paths_client (getDataFileName)

import Lenses (clientGame)

import ClientTypes (CGameStateT)

import Types 
    (Message(..), ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, 
     PotWinnersMsg, GameOverMsg, PlayersRemovedMsg, CardRevealMsg, 
     ClientGame)

main :: IO ()
main = do   
    gui <- getDataFileName "src/gui/Main.qml"

    -- BUTTONS
    foldSig <- newSignalKey :: IO (SignalKey (IO ()))
    checkSig <- newSignalKey :: IO (SignalKey (IO ()))
    callSig <- newSignalKey :: IO (SignalKey (IO ()))
    raiseSig <- newSignalKey :: IO (SignalKey (IO ()))
    allInSig <- newSignalKey :: IO (SignalKey (IO ()))

    foldS <- newIORef False
    checkS <- newIORef False
    callS <- newIORef False
    raiseS <- newIORef False
    allInS <- newIORef False

    rootClass <- newClass [
        defPropertySigRO' "foldEnabled" foldSig $ defRead foldS,
        defPropertySigRO' "checkEnabled" checkSig $ defRead checkS,
        defPropertySigRO' "callEnabled" callSig $ defRead callS,
        defPropertySigRO' "raiseEnabled" raiseSig $ defRead raiseS,
        defPropertySigRO' "allInEnabled" allInSig $ defRead allInS]

    ctx <- newObject rootClass ()

    {-
    Just testing GUI for now

    (initialState, sock) <- initialSetup

    forkIO $ evalStateT (ioLoop sock) initialState 
    -}

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    where defRead s _ = readIORef s

initialSetup :: IO (ClientGame, Socket)
initialSetup = do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect sock (addrAddress addr)

    name <- getName
    
    send sock (toStrict . encode $ name)

    msg <- recv sock 4096

    case decode msg of
        Left (_, _, err) -> error err
        Right (_, _, msg') -> case msg' of
            MIsInitialGame m -> return (m^.clientGame, sock)
            _ -> error "Invalid message recieved!"

getName :: IO String
getName = do
    putStr "Enter your name: "
    hFlush stdout
    getLine

ioLoop :: Socket -> CGameStateT ()
ioLoop sock = do
    msg <- lift $ decode <$> recv sock 4096
    case msg of
        Left (_, _, err) -> error err
        Right (_, _, msg') -> do
            handleMsg msg'
            ioLoop sock

decode :: (Binary a) => BS.ByteString -> 
                        Either (BL.ByteString, ByteOffset, String)
                               (BL.ByteString, ByteOffset, a)
decode msg = decodeOrFail $ fromStrict msg

handleMsg :: Message -> CGameStateT ()
handleMsg msg = case msg of
    MIsAction m -> handleAction m
    MIsPlayerTurn m -> handlePlayerTurn m
    MIsCard m -> handleNewCards m
    MIsDealt m -> handleMyCards m
    MIsPotWinners m -> handlePotWinners m
    MIsGameOver m -> handleGameOver m
    MIsPlayersRemoved m -> handlePlayersRemoved m
    MIsCardReveal m -> handleCardsRevealed m

handleAction :: ActionMsg a -> CGameStateT ()
handleAction = undefined

handlePlayerTurn :: PlayerTurnMsg -> CGameStateT ()
handlePlayerTurn = undefined

handleNewCards :: CardMsg -> CGameStateT ()
handleNewCards = undefined

handleMyCards :: DealtCardsMsg -> CGameStateT ()
handleMyCards = undefined

handlePotWinners :: PotWinnersMsg -> CGameStateT ()
handlePotWinners = undefined

handleGameOver :: GameOverMsg -> CGameStateT ()
handleGameOver = undefined

handlePlayersRemoved :: PlayersRemovedMsg -> CGameStateT ()
handlePlayersRemoved = undefined

handleCardsRevealed :: CardRevealMsg -> CGameStateT ()
handleCardsRevealed = undefined
