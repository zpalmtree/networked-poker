module PlayerUtilities where

import Types
import Control.Lens

numInPlay :: Game -> Int
numInPlay game = length $ filter (^.inPlay) (game^.playerInfo.players)

numAllIn :: Game -> Int
numAllIn game = length $ filter (^.allIn) (game^.playerInfo.players)

numPlayers' :: Game -> Int
numPlayers' game = game^.playerInfo.numPlayers

currentPlayerIndex :: Game -> Int
currentPlayerIndex game = game^.playerInfo.playerTurn

victorIndex :: Game -> Int
victorIndex game = head $ game^..playerInfo.players.traversed.num

advanceDealer :: Game -> Int
advanceDealer game = advance (game^.playerInfo.dealer) game

advancePlayerTurn :: Game -> Int
advancePlayerTurn game = advance (game^.playerInfo.playerTurn) game

advance :: Int -> Game -> Int
advance index' game = (index' + 1) `rem` numPlayers' game
