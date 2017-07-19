module PlayerUtilities
(
    leftOfDealer,
    setCurrentPlayer,
    getCurrentPlayer,
    numInPlay,
    numAllIn,
    nextPlayer,
    victor,
    advanceDealer,
    advancePlayerTurn,
    removeOutPlayers
)
where

import Types
import Lenses (inPlay, playerInfo, players, allIn, numPlayers, playerTurn,
               dealer, gameFinished, num, chips)

import Control.Lens
import Data.List

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

victor :: [Player] -> (Player, [Player])
victor players' = let (winners, losers) = partition (^.inPlay) players'
                  in  (head winners, losers)

advanceDealer :: Game -> Int
advanceDealer game = advance (game^.playerInfo.dealer) game

advancePlayerTurn :: Game -> Int
advancePlayerTurn game = advance (currentPlayerIndex game) game

advance :: Int -> Game -> Int
advance index' game = (index' + 1) `rem` numPlayers' game

--reset the numbers to [0..length players] and remove players with no chips
removeOutPlayers :: Game -> (Game, Maybe [Player])
removeOutPlayers game
    | newGame^.playerInfo.numPlayers <= 1 = (game & gameFinished .~ True,
                                             Just removed)
    | oldNumPlayers == newNumPlayers = (game, Nothing)
    | otherwise = (newGame, Just removed)
    where newPlayers = imap (num .~) $ 
                       filter (\x -> x^.chips > 0) (game^.playerInfo.players)
          newGame = game & playerInfo.players .~ newPlayers
                         & playerInfo.numPlayers .~ length newPlayers
          oldNumPlayers = length $ game^.playerInfo.players
          newNumPlayers = length $ newGame^.playerInfo.players
          removed = filter (\x -> x^.chips <= 0) (game^.playerInfo.players)

-- gets the player who's closest to left of dealer. This is used to give the
-- spare chips to this player in the case of a split pot.
leftOfDealer :: Game -> [Player] -> Int -> (Player, [Player])
leftOfDealer game players' n
    | not . null $ fst nearest = (head $ fst nearest, snd nearest)
    | otherwise = leftOfDealer game players' (n+1)
    where nearest = partition near players'
          near p = p^.num == (game^.playerInfo.dealer+n) `rem` numPlayers' game

nextPlayer :: Game -> Game
nextPlayer game = game & playerInfo.playerTurn .~ advancePlayerTurn game
