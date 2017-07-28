{-# LANGUAGE DeriveGeneric #-}

--hurr durr unused function go away
module RunNetwork where

import Types
import Game (gameLoop)
import Network.Socket (Socket, getAddrInfo, socket, addrFamily, addrProtocol,
                       addrSocketType, bind, addrAddress, listen, accept,
                       SockAddr, isReadable)
import Data.ByteString.Char8 (unpack)
import Network.Socket.ByteString (recv)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Aeson

run :: IO ()
run = do
    putStrLn "Starting server..."

    -- need to pick a good port at some point
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)

    putStrLn "Listening for connections..."

    listen sock 5 -- maximum number of queued connections, apparently set at 5
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
        handleMsg (eitherDecodeStrict msg) sock addr
        else putStrLn "Socket is unreadable..."

handleMsg :: Either String InitRequest -> Socket -> SockAddr -> IO ()
handleMsg (Left err) _ addr = 
    putStrLn $ printf "Couldn't decode recieved message from %s: %s..." 
                      (show addr) err
handleMsg (Right (Join name')) sock addr = join name' sock addr
handleMsg (Right (Host name')) sock addr = host name' sock addr

join :: String -> Socket -> SockAddr -> IO ()
join = undefined

host :: String -> Socket -> SockAddr -> IO ()
host = undefined

play :: IO ()
play = do
    initial <- setup
    final <- gameLoop initial
    cleanup final

setup :: IO Game
setup = undefined

cleanup :: Game -> IO ()
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

instance ToJSON InitRequest where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON InitRequest
