module TestStates
(
    testGame
)
where

import Data.UUID.Types (UUID)
import System.Random (getStdRandom, random)

import Utilities.Card (fullDeck)

import Types 
    (Game(..), Bets(..), Cards(..), Player(..), Stage(..), PlayerQueue(..))

testPlayerQueue :: IO PlayerQueue
testPlayerQueue = do
    players <- mapM testPlayer ["Jim", "Dave", "Pete", "Harry"]
    return $ PlayerQueue players 0

testPlayer :: String -> IO Player
testPlayer name = do
    uuid <- getRandomUUID
    return $ Player name uuid 1000 [] True False 0 False Nothing True

testGame :: IO Game
testGame = do
    playerQueue <- testPlayerQueue
    return $ Game playerQueue PreFlop cards' False bets' False 1

    where cards' = Cards [] fullDeck
          bets' = Bets [] 0 smallBlind bigBlind bigBlind
          bigBlind = smallBlind * 2
          smallBlind = 10

getRandomUUID :: IO UUID
getRandomUUID = getStdRandom random
