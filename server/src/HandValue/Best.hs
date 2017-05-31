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

bestFullHouse :: [Card] -> [Card]
bestFullHouse = bestX (\x -> sorted' $ handSubsets x) "No full houses found!"
    where sorted' x = sortBy sortDupesFirst $ filter isFullHouse x

bestX :: (t1 -> [t]) -> String -> t1 -> t
bestX f msg cards'
    | null sorted' = error msg
    | otherwise = head sorted'
    where sorted' = f cards'

bestFlush :: [Card] -> [Card]
bestFlush = bestX (\x -> sorted' $ handSubsets x) "No flushes found!"
    where sorted' x = sortBy (flip sortOnValue) $ filter isFlush x

bestHighCard :: [Card] -> [Card]
bestHighCard = head . sortBy (flip sortOnValue) . handSubsets

bestPair :: [Card] -> [Card]
bestPair = bestX (\x -> sorted' $ handSubsets x) "No pairs found!"
    where sorted' x = sortBy (flip sortDupesFirst) $ filter isPair x

bestTwoPair :: [Card] -> [Card]
bestTwoPair = bestX (\x -> sorted' $ handSubsets x) "No two pairs found!"
    where sorted' x = sortBy sortDupesFirst $ filter isTwoPair x

bestFourOfAKind :: [Card] -> [Card]
bestFourOfAKind = bestX (bestXOfAKind isFourOfAKind)
                  "No four of a kinds found!"

bestThreeOfAKind :: [Card] -> [Card]
bestThreeOfAKind = bestX (bestXOfAKind isThreeOfAKind) 
                   "No three of a kinds found!"

bestXOfAKind :: ([Card] -> Bool) -> [Card] -> [[Card]]
bestXOfAKind isX cards' = sortBy (flip sortXOfAKind) $ filter isX 
                                                     $ handSubsets cards'

bestStraight :: [Card] -> [Card]
bestStraight cards' = bestStraight' $ filter isStraight5Card
                                    $ handSubsets cards'

bestStraight' :: [[Card]] -> [Card]
bestStraight' straights
    | null sorted' = error "No straights found!"
    | otherwise = head sorted'
    where sorted' = sortBy (flip sortStraight) straights

bestStraightFlush :: [Card] -> [Card]
bestStraightFlush cards' = bestStraight' $ filter isStraightFlush5Card
                                         $ handSubsets cards'
