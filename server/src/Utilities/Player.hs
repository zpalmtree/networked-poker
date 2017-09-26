module Utilities.Player
(
    numInPlay,
    numAllIn,
    numPlayers,
    getCurrentPlayer,
    victorID,
    nextPlayer,
    nextDealer,
    removeOutPlayers,
    leftOfDealer
)
where

import Control.Lens (Getting, (^.), (^?!), (%=), (.=), _head)
import Control.Monad.Trans.State (get)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

import Types (Player, GameState, PlayerID)

import Lenses (inPlay, allIn, gameFinished, dealer, num, chips, players,
               playerQueue)

numInPlay :: GameState Int
numInPlay = numX inPlay

numAllIn :: GameState Int
numAllIn = numX allIn

numX :: Getting Bool Player Bool -> GameState Int
numX lens = do
    s <- get

    return . length $ filter (^.lens) (s^.playerQueue.players)

numPlayers :: GameState Int
numPlayers = do
    s <- get

    return . length $ s^.playerQueue.players

getCurrentPlayer :: GameState Player
getCurrentPlayer = do
    s <- get

    return $ s^.playerQueue.players ^?! _head

victorID :: GameState PlayerID
victorID = do
    s <- get

    return $ head (filter (^.inPlay) (s^.playerQueue.players))^.num

nextDealer :: GameState ()
nextDealer = do
    numPlayers' <- numPlayers

    playerQueue.dealer %= advance numPlayers'
    where advance n numPlayers' = n + 1 `rem` numPlayers'

nextPlayer :: GameState ()
nextPlayer = playerQueue.players %= shift
    where shift x = last x : init x

removeOutPlayers :: GameState (Maybe [Player])
removeOutPlayers = do
    s <- get

    let removed = filter (\x -> x^.chips <= 0) (s^.playerQueue.players)

    if null removed
        then return Nothing
        else do
            playerQueue.players %= remove
            oldPlayers <- flatten (s^.playerQueue.players)
            updateDealer oldPlayers

            numPlayers' <- numPlayers

            when (numPlayers' <= 1) $ gameFinished .= True

            return (Just removed)

    where remove = filter (\x -> x^.chips > 0)

updateDealer :: [Player] -> GameState ()
updateDealer old = do
    new <- get

    playerQueue.dealer .= find old (new^.playerQueue.players)

find :: Eq a => [a] -> [a] -> Int
find [] _ = 0
find (x:xs) new = fromMaybe (find xs new) (elemIndex x new)

flatten :: [a] -> GameState [a]
flatten = flatten' 0

flatten' :: Monad m => Int -> [a] -> m [a]
flatten' offset p = let (end, beginning) = splitAt offset p
                    in  return $ beginning ++ end

flattenWithOffset :: Int -> GameState [Player]
flattenWithOffset n = do
    s <- get

    let pos = s^.playerQueue.dealer + n

    numPlayers' <- numPlayers
    flatten' (pos `rem` numPlayers') (s^.playerQueue.players)

leftOfDealer :: [Player] -> GameState PlayerID
leftOfDealer subset = do
    s <- get

    flat <- flattenWithOffset 1

    return $ (subset !! find flat (s^.playerQueue.players))^.num
