module HandValue.Best
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

import Types
import HandValue.Utilities
import HandValue.Sort
import HandValue.Value
import Data.List

bestStraightFlush :: [Card] -> [Card]
bestStraightFlush = bestX sortStraight isStraightFlush5Card

bestFourOfAKind :: [Card] -> [Card]
bestFourOfAKind = bestX sortXOfAKind isFourOfAKind

bestFullHouse :: [Card] -> [Card]
bestFullHouse = bestX sortDupesFirst isFullHouse

bestFlush :: [Card] -> [Card]
bestFlush = bestX sortOnValue isFlush

bestStraight :: [Card] -> [Card]
bestStraight = bestX sortStraight isStraight5Card

bestThreeOfAKind :: [Card] -> [Card]
bestThreeOfAKind = bestX sortXOfAKind isThreeOfAKind

bestTwoPair :: [Card] -> [Card]
bestTwoPair = bestX sortDupesFirst isTwoPair

bestPair :: [Card] -> [Card]
bestPair = bestX sortDupesFirst isPair

bestHighCard :: [Card] -> [Card]
bestHighCard = bestX sortOnValue (const True)

bestX :: ([Card] -> [Card] -> Ordering) -> ([Card] -> Bool) -> [Card] -> [Card]
bestX sortFunc isFunc = maximumBy sortFunc . filter isFunc . handSubsets
