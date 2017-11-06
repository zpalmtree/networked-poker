module ClientUtilities
(
    cardToFileNameText,
    getName
)
where

import Data.Text (Text, pack)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

import Types (Card(..), Suit(..), Value(..))

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

getName :: IO String
getName = do
    putStr "Enter your name: "
    hFlush stdout
    getLine
