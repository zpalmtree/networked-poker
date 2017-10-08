module Main
(
    main
)
where

import Network.Socket 
    (Socket, SockAddr, getAddrInfo, socket, addrFamily, addrProtocol,
     addrSocketType, bind, addrAddress, listen, accept, isReadable)

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Network.Socket.ByteString (recv)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Text.Printf (printf)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State (evalStateT)

import Utilities.Player (mkNewPlayer)
import Utilities.Card (fullDeck, dealCards)
import Game (gameLoop)
import Output (outputGameOver)

import Types 
    (GameStateT, Game(..), Player(..), PlayerQueue(..), Cards(..), Bets(..),
     Stage(..))

main :: IO ()
main = do
    putStrLn "Starting server..."

    -- need to pick a good port at some point
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)

    putStrLn "Listening for connections..."

    listen sock 5 -- maximum uuidber of queued connections, apparently set at 5
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

            putStrLn $ printf "Recieved message \"%s\" from %s..." 
                            (unpack msg) (show addr)

            handleMsg (decodeOrFail $ fromStrict msg) sock addr unseated
        else putStrLn "Socket is unreadable..."

handleMsg :: Either (ByteString, ByteOffset, String) 
                    (ByteString, ByteOffset, String) 
          -> Socket -> SockAddr -> MVar [Player] -> IO ()
handleMsg (Left (_, _, err)) _ addr _ = 
    putStrLn $ printf "Couldn't decode recieved message from %s: %s..." 
                      (show addr) err

handleMsg (Right (_, _, msg)) sock _ unseated = seatPlayer sock msg unseated

seatPlayer :: Socket -> String -> MVar [Player] -> IO ()
seatPlayer sock name' unseated = do
    player <- mkNewPlayer name' sock

    -- this will block if unseated is empty, make sure to set it to [] instead
    modifyMVar_ unseated $ \a ->
        if length a - 1 == gameSize
            then do
                launchNewGame a unseated
                return []
            else return $ player : a

gameSize :: Int
gameSize = 2

launchNewGame :: [Player] -> MVar [Player] -> IO ()
launchNewGame players' playerChan = do
    let pq = PlayerQueue players' 0
        cards' = Cards [] fullDeck
        bets' = Bets [] 0 smallBlind bigBlind bigBlind
        smallBlind = 10
        bigBlind = smallBlind * 2
        game' = Game playerChan pq PreFlop cards' False bets' False 1

    void . forkIO $ evalStateT play game'

play :: GameStateT ()
play = do
    setup
    gameLoop
    cleanup

setup :: GameStateT ()
setup = dealCards

cleanup :: GameStateT ()
cleanup = outputGameOver
