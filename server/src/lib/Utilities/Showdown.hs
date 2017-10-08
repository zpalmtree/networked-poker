module Utilities.Showdown
(
    handSubsets,
    cardValue,
    cardValueAceLow,
    numOfEachValue,
    numOfSuit,
    consecutive,
    cardValues,
    sizeOfHand
)
where

import Data.List (sort, group)
import Safe (at, headNote)
import Control.Lens ((^.))

import Types (Card(..), Value(..))
import Utilities.Card (hearts, clubs, diamonds, spades)
import Lenses (value)

cardValues :: [Card] -> Bool -> [Int]
cardValues c aceHigh
    | aceHigh = sort $ map (cardValue . (^.value)) c
    | otherwise = sort $ map (cardValueAceLow . (^.value)) c

numOfSuit :: [Card] -> [Int]
numOfSuit c = map (`numSuit` c) [isHeart, isClub, isDiamond, isSpade]

numSuit :: (a -> Bool) -> [a] -> Int
numSuit f x = length $ filter f x

isHeart :: Card -> Bool
isHeart x = x `elem` hearts

isClub :: Card -> Bool
isClub x = x `elem` clubs

isDiamond :: Card -> Bool
isDiamond x = x `elem` diamonds

isSpade :: Card -> Bool
isSpade x = x `elem` spades

cardValue :: Value -> Int
cardValue Two = 2
cardValue c = 1 + cardValue (pred c)

cardValueAceLow :: Value -> Int
cardValueAceLow Ace = 1
cardValueAceLow Two = 2
cardValueAceLow c = 1 + cardValueAceLow (pred c)

-- For the 7 cards on the table, get all unique 5 card hands. There will be 21.
-- Taken from http://rosettacode.org/wiki/Combinations#Haskell
handSubsets :: [Card] -> [[Card]]
handSubsets xs = combsBySize xs `at` sizeOfHand
    where combsBySize = foldr f ([[]] : repeat [])
          f x next = zipWith (++) (map (map (x:)) ([]:next)) next

sizeOfHand :: Int
sizeOfHand = 5

consecutive :: (Eq a, Num a) => [a] -> Bool
consecutive [] = True
consecutive xs = consecutive' xs (headNote "in consecutive!" xs)

consecutive' :: (Num t, Eq t) => [t] -> t -> Bool
consecutive' [] _ = True
consecutive' (x:xs) val
    | x == val = consecutive' xs (val + 1)
    | otherwise = False

numOfEachValue :: [Card] -> [Int]
numOfEachValue cards' = map length . group . sort $ map (^.value) cards'
