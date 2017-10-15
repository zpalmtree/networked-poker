module Constants
(
    maxPlayers,
    numTCards,
    numButtons,
    cardBack
)
where

import Data.Text (Text, pack)

maxPlayers :: Int
maxPlayers = 6

numTCards :: Int
numTCards = 5

numButtons :: Int
numButtons = 5

cardBack :: Text
cardBack = pack "assets/card-back.png"
