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

import Types (Game, Player)
import Lenses (inPlay, playerInfo, depreciatedPlayers, allIn, depreciatedNumPlayers, depreciatedPlayerTurn,
               depreciatedDealer, gameFinished, num, chips)
import Control.Lens
import Data.List (partition)

numInPlay :: Game -> Int
numInPlay game = length $ filter (^.inPlay) (game^.playerInfo.depreciatedPlayers)

numAllIn :: Game -> Int
numAllIn game = length $ filter (^.allIn) (game^.playerInfo.depreciatedPlayers)

depreciatedNumPlayers' :: Game -> Int
depreciatedNumPlayers' game = game^.playerInfo.depreciatedNumPlayers

currentPlayerIndex :: Game -> Int
currentPlayerIndex game = game^.playerInfo.depreciatedPlayerTurn

getCurrentPlayer :: Game -> Player
getCurrentPlayer game = game^.playerInfo.depreciatedPlayers ^?! 
                        ix (currentPlayerIndex game)

setCurrentPlayer :: (Applicative f) => Game -> (Player -> f Player) -> Game
                                            -> f Game
setCurrentPlayer game = playerInfo.depreciatedPlayers.ix (currentPlayerIndex game)

victor :: [Player] -> (Player, [Player])
victor depreciatedPlayers' = let (winners, losers) = partition (^.inPlay) depreciatedPlayers'
                  in  (head winners, losers)

advanceDealer :: Game -> Int
advanceDealer game = advance (game^.playerInfo.depreciatedDealer) game

advancePlayerTurn :: Game -> Int
advancePlayerTurn game = advance (currentPlayerIndex game) game

advance :: Int -> Game -> Int
advance index' game = (index' + 1) `rem` depreciatedNumPlayers' game

--reset the numbers to [0..length depreciatedPlayers] and remove depreciatedPlayers with no chips
removeOutPlayers :: Game -> (Game, Maybe [Player])
removeOutPlayers game
    | newGame^.playerInfo.depreciatedNumPlayers <= 1 = (game & gameFinished .~ True,
                                             Just removed)
    | oldNumdepreciatedPlayers == newNumdepreciatedPlayers = (game, Nothing)
    | otherwise = (newGame, Just removed)
    where newdepreciatedPlayers = imap (num .~) $ 
                       filter (\x -> x^.chips > 0) (game^.playerInfo.depreciatedPlayers)
          newGame = game & playerInfo.depreciatedPlayers .~ newdepreciatedPlayers
                         & playerInfo.depreciatedNumPlayers .~ length newdepreciatedPlayers
          oldNumdepreciatedPlayers = length $ game^.playerInfo.depreciatedPlayers
          newNumdepreciatedPlayers = length $ newGame^.playerInfo.depreciatedPlayers
          removed = filter (\x -> x^.chips <= 0) (game^.playerInfo.depreciatedPlayers)

-- gets the player who's closest to left of depreciatedDealer. This is used to give the
-- spare chips to this player in the case of a split pot.
leftOfDealer :: Game -> [Player] -> Int -> (Player, [Player])
leftOfDealer game depreciatedPlayers' n
    | not . null $ fst nearest = (head $ fst nearest, snd nearest)
    | otherwise = leftOfDealer game depreciatedPlayers' (n+1)
    where nearest = partition near depreciatedPlayers'
          near p = p^.num == (game^.playerInfo.depreciatedDealer+n) `rem` depreciatedNumPlayers' game

nextPlayer :: Game -> Game
nextPlayer game = game & playerInfo.depreciatedPlayerTurn .~ advancePlayerTurn game
