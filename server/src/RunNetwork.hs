{-# LANGUAGE DeriveGeneric #-}

--remember to explicitly export later...
module RunNetwork where

import Network.Socket (Socket, getAddrInfo, socket, addrFamily, addrProtocol,
                       addrSocketType, bind, addrAddress, listen, accept,
                       SockAddr, isReadable)

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Network.Socket.ByteString (recv)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Binary (Binary, decodeOrFail)
import Data.Binary.Get (ByteOffset)

import Types (GameStateT)
import Game (gameLoop)

run :: IO ()
run = do
    putStrLn "Starting server..."

    -- need to pick a good port at some point
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)

    putStrLn "Listening for connections..."

    listen sock 5 -- maximum uuidber of queued connections, apparently set at 5
                  -- for most OS's. Need to look into. Queued connections
                  -- should be accepted very fast? Loop is very simple.

    forever $ listenForConnections sock -- quit server with ctrl+c

listenForConnections :: Socket -> IO ()
listenForConnections localSock = do
    (sock, addr) <- accept localSock

    putStrLn $ printf "Connection made on %s..." (show addr)

    void . forkIO $ handleNewClient sock addr

handleNewClient :: Socket -> SockAddr -> IO ()
handleNewClient sock addr = do
    readable <- isReadable sock

    if readable
        then do
            -- add a timer here?
            msg <- recv sock 4096

            putStrLn $ printf "Recieved message \"%s\" from %s..." 
                            (unpack msg) (show addr)

            handleMsg (decodeOrFail $ fromStrict msg) sock addr
        else putStrLn "Socket is unreadable..."

handleMsg :: Either (ByteString, ByteOffset, String) 
                    (ByteString, ByteOffset, InitRequest) 
          -> Socket -> SockAddr -> IO ()
handleMsg (Left (_, _, err)) _ addr = 
    putStrLn $ printf "Couldn't decode recieved message from %s: %s..." 
                      (show addr) err

handleMsg (Right (_, _, Join name')) sock addr = join name' sock addr
handleMsg (Right (_, _, Host name')) sock addr = host name' sock addr

join :: String -> Socket -> SockAddr -> IO ()
join = undefined

host :: String -> Socket -> SockAddr -> IO ()
host = undefined

play :: GameStateT ()
play = do
    setup
    gameLoop
    cleanup

setup :: GameStateT ()
setup = undefined

cleanup :: GameStateT ()
cleanup = undefined

data ActionPost = Fold
                | Check
                | Call
                | Raise { raiseTo :: Int }
                | AllIn
                | Disconnect
                deriving (Generic, Show)

data InitRequest = Join { name :: String }
                 | Host { name :: String }
                 deriving (Generic, Show)

instance Binary InitRequest
