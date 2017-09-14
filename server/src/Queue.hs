{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Queue
(
    makePlayerQueue,
    getCurrentPlayer,
    numPlayers,
    advanceDealer,
    advancePlayerTurn,
    removeOutPlayers,
    leftOfDealer,
    getPlayerN,
    applyPlayerN,
    applyCurrentPlayer
)
where

import Control.Lens ((^.), (&), (%~), (.~), makeLenses)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import Types (Player)
import Lenses (chips)

data Queue = Queue {
    _players :: [Player],
    _dealer :: Int
}

makeLenses ''Queue

newtype PQ a = PQ { 
    getQueue :: a
} deriving (Functor)

type PlayerQueue = PQ Queue

makePlayerQueue :: [Player] -> Int -> PlayerQueue
makePlayerQueue p d
    | null p = error "Can't have an empty player queue!"
    | d < 0 || d >= length p = error "Dealer must be in player bounds!"
    | otherwise = PQ (Queue p d)

--will error if you don't check your bounds
getPlayerN :: Int -> PlayerQueue -> Player
getPlayerN n p = (getQueue p^.players) !! n

applyPlayerN :: Int -> (Player -> Player) -> PlayerQueue -> PlayerQueue
applyPlayerN n f = fmap (players %~ modifyNth n f)

applyCurrentPlayer :: (Player -> Player) -> PlayerQueue -> PlayerQueue
applyCurrentPlayer = applyPlayerN 0

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
    | n == 0 = f x : xs
    | otherwise = x : modifyNth (n-1) f xs

getCurrentPlayer :: PlayerQueue -> Player
getCurrentPlayer = getPlayerN 0

numPlayers :: PlayerQueue -> Int
numPlayers p = length $ getQueue p^.players

advanceDealer :: PlayerQueue -> PlayerQueue
advanceDealer p = fmap (dealer %~ advance) p
    where advance n = n + 1 `rem` numPlayers p

advancePlayerTurn :: PlayerQueue -> PlayerQueue
advancePlayerTurn = fmap (players %~ shift)
    where shift x = last x : init x

removeOutPlayers :: PlayerQueue -> PlayerQueue
removeOutPlayers p
    | numPlayers p == numPlayers newPlayers = p
    | otherwise = fmap (dealer .~ updateDealer p newPlayers) newPlayers
    where newPlayers = fmap remove p
          remove z = z & players .~ filter (\x -> x^.chips > 0) (z^.players)

--put dealer and onwards at head of list, step through checking if player
--is in new list, if so make them the next dealer
updateDealer :: PlayerQueue -> PlayerQueue -> Int
updateDealer old new = find (flatten old) (getQueue new^.players)

find :: [Player] -> [Player] -> Int
-- no players left -> needs to be handled upstream
find [] _ = 0
find (x:xs) new = fromMaybe (find xs new) (elemIndex x new)

leftOfDealer :: PlayerQueue -> [Player] -> Player
leftOfDealer p subset = subset !! find flat (getQueue p^.players)
    where flat = flattenWithOffset 1 p -- because we want the people left of
                                       -- the dealer, not including dealer

flatten' :: Int -> PlayerQueue -> [Player]
flatten' offset p = let (end, beginning) = splitAt offset (getQueue p^.players)
                    in  beginning ++ end

flattenWithOffset :: Int -> PlayerQueue -> [Player]
flattenWithOffset n p = flatten' (pos `rem` numPlayers p) p
    where pos = getQueue p^.dealer + n

flatten :: PlayerQueue -> [Player]
flatten = flatten' 0
