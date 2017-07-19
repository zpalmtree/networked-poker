module RunNetwork
(
    run
)
where

import Game
import Types

import Network.Socket
import Control.Concurrent
import Control.Monad

run :: IO ()
run = do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "5000")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5
    forever $ listenForConnections sock

listenForConnections :: Socket -> IO ()
listenForConnections sock = do
    (clientSocket, clientAddress) <- accept sock
    void . forkIO $ handleNewClient clientSocket clientAddress

handleNewClient :: Monad m => t1 -> t -> m ()
handleNewClient clientSocket clientAddress = return ()
    --do some stuff with send / recv -- use bytestring versions

play :: IO ()
play = do
    initial <- setup
    final <- gameLoop initial
    cleanup final

setup = undefined
cleanup = undefined
