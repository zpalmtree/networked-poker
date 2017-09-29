{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Binary (Binary, encode)
import Network.Socket.ByteString (send)
import Data.ByteString.Lazy (toStrict)
import Text.Printf (printf)
import GHC.Generics (Generic)

import Network.Socket 
    (getAddrInfo, socket, addrFamily, addrProtocol, addrSocketType, 
     addrAddress, connect)

main :: IO ()
main = do   
    putStrLn "Sending message..."

    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect sock (addrAddress addr)

    numSent <- send sock (toStrict $ encode $ Join "dave")

    putStrLn $ printf "Sent %d bytes to %s..." numSent 
                      (show $ addrAddress addr)

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

instance Binary InitRequest where
