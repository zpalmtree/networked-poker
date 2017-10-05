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

import Data.List (sortBy)
import Safe (headNote)

import Types (Card)

import Utilities.Showdown 
    (handSubsets, numOfEachValue, numOfSuit, sizeOfHand, consecutive, 
     cardValues)

isStraightFlush7Card :: [Card] -> Bool
isStraightFlush7Card cards'
    | length cards' /= 7 = error "7 cards not passed to isStraightFlush7Card!"
    | otherwise = any isStraightFlush5Card $ handSubsets cards'

isStraightFlush5Card :: [Card] -> Bool
isStraightFlush5Card cards'
    | length cards' /= 5 = error "5 cards not passed to isStraightFlush5Card!"
    | otherwise = isStraight5Card cards' && isFlush cards'

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

-- filter for the cards which we have two or more of, sort descending, if
-- longer than two then if the head is >= 3, it must be a full house
isFullHouse :: [Card] -> Bool
isFullHouse cards'
    | length sorted' >= 2 = headNote "in isFullHouse!" sorted' >= 3
    | otherwise = False
    where pairOrAbove = filter (>=2) $ numOfEachValue cards'
          sorted' = sortBy (flip compare) pairOrAbove

isXOfAKind :: Int -> [Card] -> Bool
isXOfAKind x cards' = maximum (numOfEachValue cards') >= x

isFlush :: [Card] -> Bool
isFlush cards' = maximum (numOfSuit cards') >= sizeOfHand

-- note - if 7 cards aren't passed to this, it won't work correctly
isStraight7Card :: [Card] -> Bool
isStraight7Card c
    | length c /= 7 = error "7 cards not passed to isStraight7Card!"
    | otherwise = any isStraight5Card $ handSubsets c

isStraight5Card :: [Card] -> Bool
isStraight5Card c
    | length c /= 5 = error "5 cards not passed to isStraight5Card!"
    | otherwise = consecutive values || consecutive valuesAceLow
    where valuesAceLow = cardValues c False
          values = cardValues c True
