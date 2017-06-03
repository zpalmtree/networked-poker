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

getCurrentPlayer :: Game -> Player
getCurrentPlayer game = game^.playerInfo.players ^?! 
                        ix (currentPlayerIndex game)

setCurrentPlayer :: (Applicative f) => Game -> (Player -> f Player) -> Game
                                               -> f Game
setCurrentPlayer game = playerInfo.players.ix (currentPlayerIndex game)

victorIndex :: Game -> Int
victorIndex game = head (filter (^.inPlay) (game^.playerInfo.players))^.num

advanceDealer :: Game -> Int
advanceDealer game = advance (game^.playerInfo.dealer) game

advancePlayerTurn :: Game -> Int
advancePlayerTurn game = advance (currentPlayerIndex game) game

advance :: Int -> Game -> Int
advance index' game = (index' + 1) `rem` numPlayers' game

--reset the numbers to [0..length players] and remove players with no chips
removeOutPlayers :: Game -> Game
removeOutPlayers game
    | newGame^.playerInfo.numPlayers <= 1 = game & gameFinished .~ True
    | otherwise = newGame
    where newPlayers = imap (num .~) $ 
                       filter (\x -> x^.chips > 0) (game^.playerInfo.players)
          newGame = game & playerInfo.players .~ newPlayers
                         & playerInfo.numPlayers .~ length newPlayers
