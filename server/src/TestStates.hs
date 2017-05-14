module TestStates where

import Types
import CardUtilities

testBets :: Bets
testBets = Bets 0 0 10 20

testCards :: Cards
testCards = Cards Nothing fullDeck

testGame :: Game
testGame = Game testPlayers PreFlop testCards False testBets

testPlayer1 :: Player
testPlayer1 = Player 1 "test" 1000 Nothing True False 0

testPlayer2 :: Player
testPlayer2 = Player 2 "test2" 1000 Nothing True False 0

testPlayers :: Players
testPlayers = Players 2 [testPlayer1, testPlayer2] 1 0
