module Showdown.Best
(
    bestStraightFlush,
    bestFourOfAKind,
    bestFullHouse,
    bestFlush,
    bestStraight,
    bestThreeOfAKind,
    bestTwoPair,
    bestPair,
    bestHighCard
)
where

import Data.List (sortBy, group, sort, maximumBy)
import Safe (at, headNote)
import Control.Lens ((^.))

import Types (Card, HandInfo(..), Value(..), Hand(..))
import Utilities.Showdown (cardValueAceLow, handSubsets)
import Showdown.Ord (ordStraight, ordXOfAKind, ordOnValue, ordOnLength)
import Lenses (value)

import Showdown.Value 
    (isStraightFlush5Card, isFourOfAKind, isFullHouse, isFlush, 
     isStraight5Card, isThreeOfAKind, isTwoPair, isPair)

bestStraightFlush :: [Card] -> HandInfo
bestStraightFlush cards' = HandInfo value' topHand
    where topHand = bestX ordStraight isStraightFlush5Card cards'
          value' = uncurry StraightFlush (straightValue topHand)

bestFourOfAKind :: [Card] -> HandInfo
bestFourOfAKind cards' = HandInfo value' topHand
    where topHand = bestX ordXOfAKind isFourOfAKind cards'
          value' = FourOfAKind $ xOfAKindValue topHand

bestFullHouse :: [Card] -> HandInfo
bestFullHouse cards' = HandInfo value' topHand
    where topHand = bestX ordXOfAKind isFullHouse cards'
          value' = uncurry FullHouse $ multiPairValue topHand

bestFlush :: [Card] -> HandInfo
bestFlush cards' = HandInfo value' topHand
    where topHand = bestX ordOnValue isFlush cards'
          value' = Flush . maximum $ map (^.value) topHand

bestStraight :: [Card] -> HandInfo
bestStraight cards' = HandInfo value' topHand
    where topHand = bestX ordStraight isStraight5Card cards'
          value' = uncurry Straight $ straightValue topHand

bestThreeOfAKind :: [Card] -> HandInfo
bestThreeOfAKind cards' = HandInfo value' topHand
    where topHand = bestX ordXOfAKind isThreeOfAKind cards'
          value' = ThreeOfAKind $ xOfAKindValue topHand

bestTwoPair :: [Card] -> HandInfo
bestTwoPair cards' = HandInfo value' topHand
    where topHand = bestX ordXOfAKind isTwoPair cards'
          value' = uncurry TwoPair $ multiPairValue topHand

bestPair :: [Card] -> HandInfo
bestPair cards' = HandInfo value' topHand
    where topHand = bestX ordXOfAKind isPair cards'
          value' = Pair $ xOfAKindValue topHand

bestHighCard :: [Card] -> HandInfo
bestHighCard cards' = HandInfo value' topHand
    where topHand = bestX ordOnValue (const True) cards'
          value' = HighCard . maximum $ map (^.value) topHand

multiPairValue :: [Card] -> (Value, Value)
multiPairValue topHand = (getVal 0, getVal 1)
    where values = sortBy (flip ordOnLength) . group . sort 
                 $ map (^.value) topHand
          getVal n = headNote "in multiPairValue!" $ values `at` n

straightValue :: [Card] -> (Value, Value)
straightValue topHand
    | map (cardValueAceLow . (^.value)) topHand == [1..5] = (Ace, Five)
    | otherwise = (minimum values, maximum values)
    where values = map (^.value) topHand

xOfAKindValue :: [Card] -> Value
xOfAKindValue topHand = headNote "in XOfAKindValue!"
                        . maximumBy ordOnLength $ group values
    where values = sort $ map (^.value) topHand

bestX :: ([Card] -> [Card] -> Ordering) -> ([Card] -> Bool) -> [Card] -> [Card]
bestX sortFunc isFunc = maximumBy sortFunc . filter isFunc . handSubsets
