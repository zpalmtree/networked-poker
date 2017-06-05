module TestStates where

import Types
import CardUtilities

testBets :: Bets
testBets = Bets 0 0 10 20

testCards :: Cards
testCards = Cards Nothing fullDeck

testGame :: Game
testGame = Game testPlayers PreFlop testCards False testBets False

testPlayer1 :: Player
testPlayer1 = Player "test" 0 1000 [] True False 0 False [] Nothing

testPlayer2 :: Player
testPlayer2 = Player "test2" 1 1000 [] True False 0 False [] Nothing

testPlayers :: Players
testPlayers = Players 2 [testPlayer1, testPlayer2] 1 0
