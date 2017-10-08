module Utilities.Player
(
    numInPlay,
    numInPlayPure,
    numAllIn,
    numAllInPure,
    numPlayers,
    numPlayersT,
    getCurrentPlayerPure,
    getCurrentPlayer,
    getCurrentPlayerT,
    getPlayerByUUID,
    victorID,
    nextPlayer,
    nextPlayerT,
    nextDealer,
    nextDealerT,
    removeOutPlayers,
    leftOfDealer,
    mkNewPlayer,
    getCurrentPlayerUUID
)
where

import Control.Lens (Getting, (^.), (^?!), (%=), (.=), (^..), _head, traversed)
import Control.Monad.Trans.State (get)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
import Safe (headNote, tailNote)
import Data.UUID.Types (UUID)
import Network.Socket (Socket)
import System.Random (getStdRandom, random)

import Utilities.Types (fromPure)
import Types (Player(..), GameState, GameStateT, Game)

import Lenses (inPlay, allIn, gameFinished, dealer, uuid, chips, players,
               playerQueue)

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

victorID :: GameState UUID
victorID = do
    s <- get

    return $ headNote "in victorID!" 
                      (filter (^.inPlay) (s^.playerQueue.players))^.uuid

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

removeOutPlayers :: GameState (Maybe [UUID])
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

            return . Just $ removed^..traversed.uuid

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

leftOfDealer :: [Player] -> GameState UUID
leftOfDealer subset = do
    s <- get

    return $ findNearestToDealer subset (s^.playerQueue.players)

findNearestToDealer :: [Player] -> [Player] -> UUID
findNearestToDealer _ [] = error "No players in p:ps exist in subset!"
findNearestToDealer subset (p:ps)
    | p `elem` subset = p^.uuid
    | otherwise = findNearestToDealer subset ps

getPlayerByUUID :: UUID -> GameStateT Player
getPlayerByUUID = fromPure . getPlayerByUUIDPure

getPlayerByUUIDPure :: UUID -> GameState Player
getPlayerByUUIDPure uuid' = do
    s <- get

    unless (uuid' `elem` (s^..playerQueue.players.traversed.uuid)) $ 
        error "UUID does not belong to any known players in getPlayerByUUID!"

    let players' = filter (\p -> p^.uuid == uuid') (s^.playerQueue.players)

    return $ head players'

mkNewPlayer :: String -> Socket -> IO Player
mkNewPlayer name' sock = do
    uuid' <- getStdRandom random
    return $ Player sock name' uuid' 1000 [] True False 0 False Nothing True

getCurrentPlayerUUID :: GameStateT UUID
getCurrentPlayerUUID = do
    p <- getCurrentPlayerT
    return $ p^.uuid
