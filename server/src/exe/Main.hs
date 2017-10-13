module Main
(
    main
)
where

import Network.Socket 
    (Socket, SockAddr, getAddrInfo, socket, addrFamily, addrProtocol,
     addrSocketType, bind, addrAddress, listen, accept, isReadable)

import Data.ByteString.Lazy (ByteString, fromStrict)
import Network.Socket.ByteString (recv)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Text.Printf (printf)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)

import Utilities.Player (mkNewPlayer)
import Utilities.Card (dealCards)
import Utilities.Types (mkCGame, mkGame)
import Game (gameLoop)
import Output (outputGameOver, outputInitialGame)

import Types (GameStateT, Player(..))

main :: IO ()
main = do
    putStrLn "Starting server..."

    -- need to pick a good port at some point
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)

    putStrLn "Listening for connections..."

    listen sock 5 -- maximum number of queued connections, apparently set at 5
                  -- for most OS's. Need to look into. Queued connections
                  -- should be accepted very fast? Loop is very simple.

    unseated <- newMVar []

    forever $ listenForConnections sock unseated -- quit server with ctrl+c

listenForConnections :: Socket -> MVar [Player] -> IO ()
listenForConnections localSock unseated = do
    (sock, addr) <- accept localSock

    putStrLn $ printf "Connection made on %s..." (show addr)

    void . forkIO $ handleNewClient sock addr unseated

handleNewClient :: Socket -> SockAddr -> MVar [Player] -> IO ()
handleNewClient sock addr unseated = do
    readable <- isReadable sock

    if readable
        then do
            -- add a timer here?
            msg <- recv sock 4096

            handleMsg (decodeOrFail $ fromStrict msg) sock addr unseated
        else putStrLn "Socket is unreadable..."

handleMsg :: Either (ByteString, ByteOffset, String) 
                    (ByteString, ByteOffset, String) 
          -> Socket -> SockAddr -> MVar [Player] -> IO ()
handleMsg (Left (_, _, err)) _ addr _ = 
    putStrLn $ printf "Couldn't decode recieved message from %s: %s..." 
                      (show addr) err

handleMsg (Right (_, _, msg)) sock addr unseated = do
    putStrLn $ printf "Recieved message from %s: %s..." (show addr) msg
    seatPlayer sock msg unseated

seatPlayer :: Socket -> String -> MVar [Player] -> IO ()
seatPlayer sock name' unseated = do
    player <- mkNewPlayer name' sock

    -- this will block if unseated is empty, make sure to set it to [] instead
    modifyMVar_ unseated $ \a -> do
        putStrLn $ printf "There are %d players waiting to be seated..." 
                  (length a + 1)

        if length a == (gameSize - 1)
            then do
                launchNewGame (player : a) unseated
                return []
            else return $ player : a

gameSize :: Int
gameSize = 2

launchNewGame :: [Player] -> MVar [Player] -> IO ()
launchNewGame players' playerChan = do
    putStrLn "Making new game..."

    let game' = mkGame players' playerChan

    void . forkIO $ evalStateT play game'

play :: GameStateT ()
play = do
    setup
    gameLoop
    cleanup

setup :: GameStateT ()
setup = do
    cgame <- mkCGame

    lift $ putStrLn "Sending initial game to clients..."

    outputInitialGame cgame
    dealCards

cleanup :: GameStateT ()
cleanup = outputGameOver
