module Utilities.Player
(
    numInPlay,
    numInPlayPure,
    numAllIn,
    numAllInPure,
    numPlayers,
    numPlayersT,
    getPlayerN,
    getPlayerNT,
    getCurrentPlayerPure,
    getCurrentPlayer,
    getCurrentPlayerT,
    victorID,
    nextPlayer,
    nextPlayerT,
    nextDealer,
    nextDealerT,
    removeOutPlayers,
    leftOfDealer
)
where

import Control.Lens (Getting, (^.), (^?!), (%=), (.=), _head)
import Control.Monad.Trans.State (get)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Safe (at, headNote, tailNote)

import Utilities.Types (fromPure)
import Types (Player, GameState, GameStateT, PlayerID, Game)

import Lenses (inPlay, allIn, gameFinished, dealer, num, chips, players,
               playerQueue)

getPlayerNT :: PlayerID -> GameStateT Player
getPlayerNT = fromPure . getPlayerN

getPlayerN :: PlayerID -> GameState Player
getPlayerN n = do
    s <- get
    num' <- numPlayers
    when (n > num') $ error "Invalid index in getPlayerN"
    return $ (s^.playerQueue.players) `at` n

numInPlayPure :: Game -> Int
numInPlayPure = numXPure inPlay

numAllInPure :: Game -> Int
numAllInPure = numXPure allIn

numXPure :: Getting Bool Player Bool -> Game -> Int
numXPure lens s = length $ filter (^.lens) (s^.playerQueue.players)

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

numPlayersT :: GameStateT Int
numPlayersT = fromPure numPlayers

getCurrentPlayerPure :: Game -> Player
getCurrentPlayerPure s = s^.playerQueue.players ^?! _head

getCurrentPlayer :: GameState Player
getCurrentPlayer = do
    s <- get

    return $ s^.playerQueue.players ^?! _head

getCurrentPlayerT :: GameStateT Player
getCurrentPlayerT = fromPure getCurrentPlayer

victorID :: GameState PlayerID
victorID = do
    s <- get

    return $ headNote "in victorID!" 
                      (filter (^.inPlay) (s^.playerQueue.players))^.num

nextDealerT :: GameStateT ()
nextDealerT = fromPure nextDealer

nextDealer :: GameState ()
nextDealer = do
    numPlayers' <- numPlayers

    playerQueue.dealer %= advance numPlayers'
    where advance n numPlayers' = n + 1 `rem` numPlayers'

nextPlayer :: GameState ()
nextPlayer = playerQueue.players %= shift
    where shift x = tailNote "in nextPlayer!" x ++ [headNote "in nextPlayer!" x]

nextPlayerT :: GameStateT ()
nextPlayerT = fromPure nextPlayer

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

leftOfDealer :: [Player] -> GameState PlayerID
leftOfDealer subset = do
    s <- get

    return $ findNearestToDealer subset (s^.playerQueue.players)

findNearestToDealer :: [Player] -> [Player] -> Int
findNearestToDealer _ [] = error "No players in p:ps exist in subset!"
findNearestToDealer subset (p:ps)
    | p `elem` subset = p^.num
    | otherwise = find subset ps
