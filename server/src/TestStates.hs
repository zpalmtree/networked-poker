module TestStates
(
    testPlayer1,
    testPlayer2,
    testPlayer3,
    testPlayer4,
    testGame,
    initialPlayers,
    initialGame,
    initialPlayerQueue
)
where

import Types (Game(..), Bets(..), Cards(..), Player(..), Stage(..), PlayerQueue)

import Utilities.Card (fullDeck)

testBets :: Bets
testBets = Bets [] 0 10 20 20

testCards :: Cards
testCards = Cards [] fullDeck

testGame :: Game
testGame = Game testPlayerQueue PreFlop testCards False testBets False 1

testPlayerQueue :: PlayerQueue
testPlayerQueue = PlayerQueue 
                  [testPlayer1, testPlayer2, testPlayer3, testPlayer4] 0

testPlayer1 :: Player
testPlayer1 = Player "test" 0 1000 [] True False 0 False [] Nothing True

testPlayer2 :: Player
testPlayer2 = Player "test2" 1 1000 [] True False 0 False [] Nothing True

testPlayer3 :: Player
testPlayer3 = Player "test3" 2 1000 [] True False 0 False [] Nothing True

testPlayer4 :: Player
testPlayer4 = Player "test4" 3 1000 [] True False 0 False [] Nothing True

initialPlayerQueue :: [Player] -> PlayerQueue
initialPlayerQueue p = makePlayerQueue p 0

initialGame :: Int -> Players -> PlayerQueue -> Game
initialGame smallBlind players' playerQueue = 
    Game playerQueue PreFlop cards' False bets' False 1
    where cards' = Cards [] fullDeck
          bets' = Bets [] 0 smallBlind bigBlind bigBlind
          bigBlind = smallBlind * 2
