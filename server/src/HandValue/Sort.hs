module HandValue.Sort
(
    sortDupesFirst,
    sortOnValue,
    sortXOfAKind,
    sortStraight
)
where

import Types
import HandValue.Utilities
import Data.List
import Data.Function

sortDupesFirst :: [Card] -> [Card] -> Ordering
sortDupesFirst c1 c2 = groupedSort (grouped c1) (grouped c2)
    where grouped c = group $ sort $ map (cardValue . getValue) c

{- sort first by length -> three of a kind more important than two of a kind,
then sort by actual value 
e.g. [[King, King], Ace, Queen, Two] -}
groupedSort :: Ord a => [a] -> [a] -> Ordering
groupedSort x x2
    | length x > length x2 = GT
    | length x < length x2 = LT
    | otherwise = compare (sort x) (sort x2)

sortOnValue :: [Card] -> [Card] -> Ordering
sortOnValue c1 c2 = compare (values c1) (values c2)
    where values x = sort $ map (cardValue . getValue) x

sortXOfAKind :: [Card] -> [Card] -> Ordering
sortXOfAKind xOfAKind1 xOfAKind2
    | sortOnLength xOfAKind1 > sortOnLength xOfAKind2 = GT
    | sortOnLength xOfAKind1 < sortOnLength xOfAKind2 = LT
    | otherwise = compare (compareKickers xOfAKind1) (compareKickers xOfAKind2)
    where sortOnLength x = sortBy (flip $ on compare length) $ grouped x
          grouped c = group $ sort $ map (cardValue . getValue) c
          compareKickers x = sortBy (flip compare) $ sortOnLength x

sortStraight :: [Card] -> [Card] -> Ordering
sortStraight straight1 straight2 = compare (straightKicker straight1) 
                                           (straightKicker straight2)
    where aceLow x = map (cardValueAceLow . getValue) x
          straightKicker straight'
            | aceLow straight' == [1..5] = maximum $ aceLow straight'
            | otherwise = maximum $ map (cardValue . getValue) straight'
