{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Binary (Binary, encode, decodeOrFail)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Binary.Get (ByteOffset)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict, fromStrict)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Data.UUID.Types (UUID)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrProtocol, addrSocketType, 
     addrAddress, connect)

main :: IO ()
main = do   
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect sock (addrAddress addr)

    putStr "Enter your name: "
    hFlush stdout
    input <- getLine

    send sock (toStrict . encode $ input)
    
    forever (ioLoop sock)

ioLoop :: Socket -> IO ()
ioLoop sock = do
    msg <- decode <$> recv sock 4096
    case msg of
        Left (_, _, err) -> putStrLn err
        Right (_, _, msg') -> print (msg' :: ActionMessage Int)

decode :: (Binary a) => BS.ByteString -> 
                        Either (BL.ByteString, ByteOffset, String)
                               (BL.ByteString, ByteOffset, a)
decode msg = decodeOrFail $ fromStrict msg

data Action a = Fold 
              | Check 
              | Call 
              | Raise Int
              | AllIn 
              deriving (Generic, Show)

data ActionMessage a = ActionMessage {
    _action :: Action a,
    _player :: UUID
} deriving (Generic, Show)

instance Binary (ActionMessage a)
instance Binary (Action a)
