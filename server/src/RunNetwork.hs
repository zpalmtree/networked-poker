module RunNetwork
(
    run
)
where

import Game (gameLoop)

import Network.Socket (Socket, getAddrInfo, socket, addrFamily, addrProtocol,
                       addrSocketType, bind, addrAddress, listen, accept)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)

run :: IO ()
run = do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "5000")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5 -- maximum number of queued connections, apparently set at 5
                  -- for most OS's. Need to look into. Queued connections
                  -- should be accepted very fast? Loop is very simple.
    forever $ listenForConnections sock -- quit server with ctrl+c

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
