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
    applyCurrentPlayer,
    applyPlayers
)
where

import Control.Lens ((^.), (&), (%~), (.~))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import Types (Player, PlayerQueue(..))
import Lenses (chips, players, dealer)

makePlayerQueue :: [Player] -> Int -> PlayerQueue
makePlayerQueue p d
    | null p = error "Can't have an empty player queue!"
    | d < 0 || d >= length p = error "Dealer must be in player bounds!"
    | otherwise = PlayerQueue p d

--will error if you don't check your bounds
getPlayerN :: Int -> PlayerQueue -> Player
getPlayerN n p
    | length (p^.players) > n = (p^.players) !! n
    | otherwise = error "Invalid player index given!"

applyPlayerN :: Int -> (Player -> Player) -> PlayerQueue -> PlayerQueue
applyPlayerN n f p = p & (players %~ modifyNth n f)

applyCurrentPlayer :: (Player -> Player) -> PlayerQueue -> PlayerQueue
applyCurrentPlayer = applyPlayerN 0

applyPlayers :: ([Player] -> [Player]) -> PlayerQueue -> PlayerQueue
applyPlayers f p = p & players %~ f

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
    | n == 0 = f x : xs
    | otherwise = x : modifyNth (n-1) f xs

getCurrentPlayer :: PlayerQueue -> Player
getCurrentPlayer = getPlayerN 0

numPlayers :: PlayerQueue -> Int
numPlayers p = length $ p^.players

advanceDealer :: PlayerQueue -> PlayerQueue
advanceDealer p = p & dealer %~ advance
    where advance n = n + 1 `rem` numPlayers p

advancePlayerTurn :: PlayerQueue -> PlayerQueue
advancePlayerTurn p = p & players %~ shift
    where shift x = last x : init x

removeOutPlayers :: PlayerQueue -> PlayerQueue
removeOutPlayers p
    | numPlayers p == numPlayers newPlayers = p
    | otherwise = newPlayers & dealer .~ updateDealer p newPlayers
    where newPlayers = remove p
          remove z = z & players .~ filter (\x -> x^.chips > 0) (z^.players)

--put dealer and onwards at head of list, step through checking if player
--is in new list, if so make them the next dealer
updateDealer :: PlayerQueue -> PlayerQueue -> Int
updateDealer old new = find (flatten old) (new^.players)

find :: [Player] -> [Player] -> Int
-- no players left -> needs to be handled upstream
find [] _ = 0
find (x:xs) new = fromMaybe (find xs new) (elemIndex x new)

leftOfDealer :: PlayerQueue -> [Player] -> Player
leftOfDealer p subset = subset !! find flat (p^.players)
    where flat = flattenWithOffset 1 p -- because we want the people left of
                                       -- the dealer, not including dealer

flatten' :: Int -> PlayerQueue -> [Player]
flatten' offset p = let (end, beginning) = splitAt offset (p^.players)
                    in  beginning ++ end

flattenWithOffset :: Int -> PlayerQueue -> [Player]
flattenWithOffset n p = flatten' (pos `rem` numPlayers p) p
    where pos = p^.dealer + n

flatten :: PlayerQueue -> [Player]
flatten = flatten' 0

{-
zoomPlayers :: (Functor f) => ([Player] -> f [Player]) -> Game -> f Game
zoomPlayers f s = return $ s & playerQueue %~ (\p -> applyPlayers f p)
-}
