module Utilities
(
    getName,
    decode,
    cardToFileNameText
)
where

import Data.Text (Text, pack)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Types (Message, Card(..), Suit(..), Value(..))

getName :: IO String
getName = do
    putStr "Enter your name: "
    hFlush stdout
    getLine

decode :: BS.ByteString -> Either (BL.ByteString, ByteOffset, String)
                                  (BL.ByteString, ByteOffset, Message)
decode msg = decodeOrFail $ fromStrict msg

cardToFileNameText :: Card -> Text
cardToFileNameText = pack . cardToFileName

cardToFileName :: Card -> String
cardToFileName (Card v s) = printf "assets/card-%s-%ss.png" v' s'
    where v' = valueToFileName v
          s' = suitToFileName s

valueToFileName :: Value -> String
valueToFileName Two = "2"
valueToFileName Three = "3"
valueToFileName Four = "4"
valueToFileName Five = "5"
valueToFileName Six = "6"
valueToFileName Seven = "7"
valueToFileName Eight = "8"
valueToFileName Nine = "9"
valueToFileName Ten = "10"
valueToFileName Jack = "jack"
valueToFileName Queen = "queen"
valueToFileName King = "king"
valueToFileName Ace = "ace"

suitToFileName :: Suit -> String
suitToFileName Club = "club"
suitToFileName Diamond = "diamond"
suitToFileName Heart = "heart"
suitToFileName Spade = "spade"
