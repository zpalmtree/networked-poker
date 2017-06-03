module HandValue.Sort
(
    sortOnValue,
    sortXOfAKind,
    sortStraight
)
where

import Types
import HandValue.Utilities
import Data.List

sortOnValue :: [Card] -> [Card] -> Ordering
sortOnValue c1 c2 = compare (values c1) (values c2)
    where values x = sort $ map (cardValue . getValue) x

sortXOfAKind :: [Card] -> [Card] -> Ordering
sortXOfAKind xOfAKind1 xOfAKind2 = compare (sortOnLength xOfAKind1) 
                                           (sortOnLength xOfAKind2)
    where sortOnLength x = sortBy (flip ordOnLength) $ grouped x
          grouped c = group . sort $ map (cardValue . getValue) c

ordOnLength :: (Ord (t a), Foldable t) => t a -> t a -> Ordering
ordOnLength x1 x2
    | length x1 > length x2 = GT
    | length x1 < length x2 = LT
    | otherwise = compare x1 x2

sortStraight :: [Card] -> [Card] -> Ordering
sortStraight straight1 straight2 = compare (straightKicker straight1) 
                                           (straightKicker straight2)
    where aceLow = map (cardValueAceLow . getValue)
          straightKicker straight'
            | aceLow straight' == [1..5] = maximum $ aceLow straight'
            | otherwise = maximum $ map (cardValue . getValue) straight'
