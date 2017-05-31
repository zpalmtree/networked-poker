module HandValue
(
    handValue,
    isStraightFlush,
    isFourOfAKind,
    isFullHouse,
    isFlush,
    isStraight,
    isThreeOfAKind,
    isTwoPair,
    isPair,
    straightKicker
)
where

import Types
import CardUtilities
import Data.List
import Data.Function

straightKicker :: [Card] -> Int
straightKicker cards' = maximum $ map straightKicker' straights
    where straights = filter isStraight (handSubsets cards')

straightKicker' :: [Card] -> Int
straightKicker' straight'
    | valuesAceLow == [1..5] = maximum valuesAceLow
    | otherwise = maximum $ map (cardValue . getValue) straight'
    where valuesAceLow = map (cardValueAceLow . getValue) straight'

handValue :: [Card] -> Hand
handValue cards'
    | isStraightFlush cards' = StraightFlush
    | isFourOfAKind cards' = FourOfAKind
    | isFullHouse cards' = FullHouse
    | isFlush cards' = Flush
    | isStraight cards' = Straight
    | isThreeOfAKind cards' = ThreeOfAKind
    | isTwoPair cards' = TwoPair
    | isPair cards' = Pair
    | otherwise = HighCard

isStraightFlush :: [Card] -> Bool
isStraightFlush cards' = any (\x -> isStraight x && isFlush x)
                             (handSubsets cards')

isFourOfAKind :: [Card] -> Bool
isFourOfAKind = isXOfAKind 4

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind = isXOfAKind 3

isTwoPair :: [Card] -> Bool
isTwoPair cards' = (length . filter (>=2) $ numOfEachValue cards') >= 2

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
isXOfAKind x cards' = maximum (numOfEachValue cards') == x

numOfEachValue :: [Card] -> [Int]
numOfEachValue cards' = map length . group . sort $ map getValue cards'

isFlush :: [Card] -> Bool
isFlush cards' = maximum (numOfSuit cards') >= sizeOfHand

-- note - if 7 cards aren't passed to this, it won't work correctly
isStraight :: [Card] -> Bool
isStraight c = isStraight' x || isStraight' x' || isStraight' x''
    where x = take 5 fixed
          x' = take 5 $ drop 1 fixed
          x'' = drop 2 fixed
          fixed = sorted c

isStraight' :: [Card] -> Bool
isStraight' c = consecutive values || consecutive valuesAceLow
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
