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
import Safe (at)
import Control.Lens ((^.))

import Types (Card(..), Value(..))
import Utilities.Card (hearts, clubs, diamonds, spades)
import Lenses (value)

cardValues :: [Card] -> Bool -> [Int]
cardValues c True = sort $ map (cardValue . (^.value)) c
cardValues c _ = sort $ map (cardValueAceLow . (^.value)) c

numOfSuit :: [Card] -> [Int]
numOfSuit c = map (`numSuit` c) [isHeart, isClub, isDiamond, isSpade]

numSuit :: (a -> Bool) -> [a] -> Int
numSuit = (length .) . filter

isHeart :: Card -> Bool
isHeart = (`elem` hearts)

isClub :: Card -> Bool
isClub = (`elem` clubs)

isDiamond :: Card -> Bool
isDiamond = (`elem` diamonds)

isSpade :: Card -> Bool
isSpade = (`elem` spades)

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

-- from https://stackoverflow.com/a/15542475/8737306
consecutive :: (Eq a, Enum a) => [a] -> Bool
consecutive xs = and $ zipWith ((==) . succ) xs (tail xs)

numOfEachValue :: [Card] -> [Int]
numOfEachValue = map length . group . sort . map (^.value)
