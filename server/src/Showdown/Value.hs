module Showdown.Value
(
    isStraightFlush7Card,
    isStraightFlush5Card,
    isFourOfAKind,
    isFullHouse,
    isFlush,
    isStraight7Card,
    isStraight5Card,
    isThreeOfAKind,
    isTwoPair,
    isPair
)
where

import Types
import Showdown.Utilities

import Data.List

isStraightFlush7Card :: [Card] -> Bool
isStraightFlush7Card cards' = any isStraightFlush5Card $ handSubsets cards'

isStraightFlush5Card :: [Card] -> Bool
isStraightFlush5Card cards' = isStraight5Card cards' && isFlush cards'

isFourOfAKind :: [Card] -> Bool
isFourOfAKind = isXOfAKind 4

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind = isXOfAKind 3

isTwoPair :: [Card] -> Bool
isTwoPair cards'
    | (length . filter (>=2) $ numOfEachValue cards') >= 2 = True
    | otherwise = any (>=4) $ numOfEachValue cards'

isPair :: [Card] -> Bool
isPair = isXOfAKind 2

{- filter for the cards which we have two or more of, sort descending, if
longer than two then if the head is >= 3, it must be a full house -}
isFullHouse :: [Card] -> Bool
isFullHouse cards'
    | length sorted' >= 2 = head sorted' >= 3
    | otherwise = False
    where pairOrAbove = filter (>=2) $ numOfEachValue cards'
          sorted' = sortBy (flip compare) pairOrAbove

isXOfAKind :: Int -> [Card] -> Bool
isXOfAKind x cards' = maximum (numOfEachValue cards') >= x

isFlush :: [Card] -> Bool
isFlush cards' = maximum (numOfSuit cards') >= sizeOfHand

-- note - if 7 cards aren't passed to this, it won't work correctly
isStraight7Card :: [Card] -> Bool
isStraight7Card c = isStraight5Card x || isStraight5Card x' 
                                      || isStraight5Card x''
    where x = take 5 fixed
          x' = take 5 $ drop 1 fixed
          x'' = drop 2 fixed
          fixed = sorted c

isStraight5Card :: [Card] -> Bool
isStraight5Card c = consecutive values || consecutive valuesAceLow
    where valuesAceLow = cardValues c False
          values = cardValues c True
