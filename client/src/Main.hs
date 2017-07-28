{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding,
                   defaultOptions, encode)
import Network.Socket (getAddrInfo, socket, addrFamily, addrProtocol,
                       addrSocketType, addrAddress, connect)
import Network.Socket.ByteString (send)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Text.Printf (printf)
import GHC.Generics (Generic)

main :: IO ()
main = do   
    putStrLn "Sending message..."
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    let join = Join "dave"
    numSent <- send sock (encodeStrict join)
    putStrLn $ printf "Sent %d bytes to %s..." numSent (show $ addrAddress addr)

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

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode
