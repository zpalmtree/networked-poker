module Showdown.Ord
(
    ordOnValue,
    ordXOfAKind,
    ordStraight,
    ordOnLength,
    ordHand
)
where

import Types (Card, HandInfo(..), Hand(..))
import Showdown.Utilities (getValue, cardValue, cardValueAceLow)
import Data.List (sort, group, sortBy)

ordOnValue :: [Card] -> [Card] -> Ordering
ordOnValue c1 c2 = compare (values c1) (values c2)
    where values x = sort $ map (cardValue . getValue) x

ordXOfAKind :: [Card] -> [Card] -> Ordering
ordXOfAKind xOfAKind1 xOfAKind2 = compare (sortOnLength xOfAKind1) 
                                          (sortOnLength xOfAKind2)
    where sortOnLength x = sortBy (flip ordOnLength) $ grouped x
          grouped c = group . sort $ map (cardValue . getValue) c

ordOnLength :: (Ord (t a), Foldable t) => t a -> t a -> Ordering
ordOnLength x1 x2
    | length x1 > length x2 = GT
    | length x1 < length x2 = LT
    | otherwise = compare x1 x2

ordStraight :: [Card] -> [Card] -> Ordering
ordStraight straight1 straight2 = compare (straightKicker straight1) 
                                           (straightKicker straight2)
    where aceLow = map (cardValueAceLow . getValue)
          straightKicker straight'
            | aceLow straight' == [1..5] = maximum $ aceLow straight'
            | otherwise = maximum $ map (cardValue . getValue) straight'

ordHand :: HandInfo -> HandInfo -> Ordering
ordHand (HandInfo a _) (HandInfo b _)
    | a > b = GT
    | a < b = LT

ordHand (HandInfo (HighCard a) c1) (HandInfo (HighCard b) c2)
    | a > b = GT
    | a < b = LT
    | otherwise = compare (map getValue c1) (map getValue c2)

ordHand h1@(HandInfo (Pair a) _) h2@(HandInfo (Pair b) _)
    | a > b = GT
    | a < b = LT
    | otherwise = compareKickers h1 h2

ordHand h1@(HandInfo (TwoPair a b) _) h2@(HandInfo (TwoPair c d) _)
    | a > c = GT
    | a < c = LT
    | b > d = GT
    | b < d = LT
    | otherwise = compareKickers h1 h2

ordHand h1@(HandInfo (ThreeOfAKind a) _) h2@(HandInfo (ThreeOfAKind b) _)
    | a > b = GT
    | a < b = LT
    | otherwise = compareKickers h1 h2

-- 5 card hand, no kickers to compare
ordHand (HandInfo (Straight _ a) _) (HandInfo (Straight _ b) _) = compare a b

-- 5 card hand, no kickers to compare
ordHand (HandInfo (Flush a) _) (HandInfo (Flush b) _) = compare a b

-- in poker, these will never be equal because there aren't enough cards
ordHand (HandInfo (FullHouse a b) _) (HandInfo (FullHouse c d) _)
    | a > c = GT
    | a < c = LT
    | b > d = GT
    | b < d = LT
    | otherwise = EQ

ordHand h1@(HandInfo (FourOfAKind a) _) h2@(HandInfo (FourOfAKind b) _)
    | a > b = GT
    | a < b = LT
    | otherwise = compareKickers h1 h2

-- 5 card hand, no kickers to compare
ordHand (HandInfo (StraightFlush _ a) _) (HandInfo (StraightFlush _ b) _) 
    = compare a b

ordHand _ _ = error "Mismatched equal types in ordHand"

compareKickers :: HandInfo -> HandInfo -> Ordering
compareKickers (HandInfo value1 hand1) (HandInfo value2 hand2)
    | isPair value1 value2 ||
      isThreeOfAKind value1 value2 ||
      isFourOfAKind value1 value2 = compare (multiSameCardFix hand1) 
                                            (multiSameCardFix hand2)

    | isTwoPair value1 value2 = compare (twoPairFix hand1) (twoPairFix hand2)

    | otherwise = error "Unexpected cards in compareKickers"
    where multiSameCardFix xs = tail . group . sort $ map getValue xs
          -- pair fix removes the first group of same cards, i.e. the first
          -- pair, three of a kind, four of a kind, whatever
          -- it's still nested in a list, but whatever, compare works fine

          twoPairFix xs = last . group . sort $ map getValue xs
          -- two pair fix just takes the last item, cause 5 cards minus two
          -- pairs just leaves one card, wrapped in a list

isPair :: Hand a b -> Hand c d -> Bool
isPair (Pair _) (Pair _) = True
isPair _ _ = False

isTwoPair :: Hand a b -> Hand c d -> Bool
isTwoPair (TwoPair _ _) (TwoPair _ _) = True
isTwoPair _ _ = False

isThreeOfAKind :: Hand a b -> Hand c d -> Bool
isThreeOfAKind (ThreeOfAKind _) (ThreeOfAKind _) = True
isThreeOfAKind _ _ = False

isFourOfAKind :: Hand a b -> Hand c d -> Bool
isFourOfAKind (FourOfAKind _) (FourOfAKind _) = True
isFourOfAKind _ _ = False
