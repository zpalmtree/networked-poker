module GameUtilities where

import Types
import PlayerUtilities
import CardUtilities
import BetUtilities

testGame :: Game
testGame = Game testPlayers PreFlop testCards False testBets
