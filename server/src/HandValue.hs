module HandValue
(
    handValue,
    straightFlush,
    fourOfAKind,
    fullHouse,
    flush,
    straight,
    threeOfAKind,
    twoPair,
    pair
)
where

import Types
import CardUtilities
import Data.List
import Data.Function

handValue :: [Card] -> Hand
handValue cards'
    | straightFlush cards' = StraightFlush
    | fourOfAKind cards' = FourOfAKind
    | fullHouse cards' = FullHouse
    | flush cards' = Flush
    | straight cards' = Straight
    | threeOfAKind cards' = ThreeOfAKind
    | twoPair cards' = TwoPair
    | pair cards' = Pair
    | otherwise = HighCard

straightFlush :: [Card] -> Bool
straightFlush cards' = any (\x -> straight x && flush x) (handSubsets cards')

fourOfAKind :: [Card] -> Bool
fourOfAKind = xOfAKind 4

threeOfAKind :: [Card] -> Bool
threeOfAKind = xOfAKind 3

twoPair :: [Card] -> Bool
twoPair cards' = (length . filter (>=2) $ numOfEachValue cards') >= 2

pair :: [Card] -> Bool
pair = xOfAKind 2

{- filter for the cards which we have two or more of, sort descending, if
longer than two then if the head is >= 3, it must be a full house -}
fullHouse :: [Card] -> Bool
fullHouse cards'
    | length sorted' >= 2 = head sorted' >= 3
    | otherwise = False
    where pairOrAbove = filter (>=2) $ numOfEachValue cards'
          sorted' = sortBy (flip compare) pairOrAbove

xOfAKind :: Int -> [Card] -> Bool
xOfAKind x cards' = maximum (numOfEachValue cards') == x

numOfEachValue :: [Card] -> [Int]
numOfEachValue cards' = map length . group . sort $ map getValue cards'

flush :: [Card] -> Bool
flush cards' = maximum (numOfSuit cards') >= sizeOfHand

-- note - if 7 cards aren't passed to this, it won't work correctly
straight :: [Card] -> Bool
straight c = isStraight x || isStraight x' || isStraight x''
    where x = take 5 fixed
          x' = take 5 $ drop 1 fixed
          x'' = drop 2 fixed
          fixed = sorted c

isStraight :: [Card] -> Bool
isStraight c = consecutive values || consecutive valuesAceLow
    where valuesAceLow = cardValues c False
          values = cardValues c True

cardValues :: [Card] -> Bool -> [Int]
cardValues c aceHigh
    | aceHigh = sort $ map (cardValue . getValue) c
    | otherwise = sort $ map (cardValueAceLow . getValue) c

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

sorted :: [Card] -> [Card]
sorted = sortBy (compare `on` getValue)

getValue :: Card -> Value
getValue (Card v _) = v

cardValue :: Value -> Int
cardValue Two = 2
cardValue c = 1 + cardValue (pred c)

cardValueAceLow :: Value -> Int
cardValueAceLow Ace = 1
cardValueAceLow Two = 2
cardValueAceLow c = 1 + cardValueAceLow (pred c)

--For the 7 cards on the table, get all unique 5 card hands. There will be 21.
--taken from http://rosettacode.org/wiki/Combinations#Haskell
handSubsets :: [Card] -> [[Card]]
handSubsets xs = combsBySize xs !! sizeOfHand
    where combsBySize = foldr f ([[]] : repeat [])
          f x next = zipWith (++) (map (map (x:)) ([]:next)) next

sizeOfHand :: Int
sizeOfHand = 5

consecutive :: (Eq a, Num a) => [a] -> Bool
consecutive [] = True
consecutive xs = consecutive' xs (head xs)

consecutive' :: (Num t, Eq t) => [t] -> t -> Bool
consecutive' [] _ = True
consecutive' (x:xs) val
    | x == val = consecutive' xs (val + 1)
    | otherwise = False
