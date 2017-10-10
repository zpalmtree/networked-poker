module Main
(
    main
)
where

import Data.Binary (Binary, encode, decodeOrFail)
import Data.Binary.Get (ByteOffset)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict, fromStrict)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Control.Concurrent (forkIO)

import Paths_client (getDataFileName)

import Graphics.QML
    (initialDocument, contextObject, newClass, newObject, runEngineLoop,
     defaultEngineConfig, fileDocument, anyObjRef)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrProtocol, addrSocketType, 
     addrAddress, connect)

import Lenses (cgame)

import Types 
    (Message(..), ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, 
     PotWinnersMsg, GameOverMsg, PlayersRemovedMsg, CardRevealMsg, 
     CGameStateT, ClientGame)

main :: IO ()
main = do   
    gui <- getDataFileName "src/gui/Main.qml"

    {-
    Just testing GUI for now

    (initialState, sock) <- initialSetup

    forkIO $ evalStateT (ioLoop sock) initialState 
    -}

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument gui
    }

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
            MIsInitialGame m -> return (m^.cgame, sock)
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
