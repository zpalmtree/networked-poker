module HandValue
(
    getWinners
)
where

import Types
import HandValue.Value
import HandValue.Best
import HandValue.Sort

import Control.Lens
import Data.Maybe
import Data.List
import Data.Function

bestHand :: [Card] -> (Hand, [Card])
bestHand cards'
    | isStraightFlush7Card cards' = (StraightFlush, bestStraightFlush cards')
    | isFourOfAKind cards' = (FourOfAKind, bestFourOfAKind cards')
    | isFullHouse cards' = (FullHouse, bestFullHouse cards')
    | isFlush cards' = (Flush, bestFlush cards')
    | isStraight7Card cards' = (Straight, bestStraight cards')
    | isThreeOfAKind cards' = (ThreeOfAKind, bestThreeOfAKind cards')
    | isTwoPair cards' = (TwoPair, bestTwoPair cards')
    | isPair cards' = (Pair, bestPair cards')
    | otherwise = (HighCard, bestHighCard cards')

getHandValue :: Game -> Game
getHandValue game = game & playerInfo.players.traversed %~ getCardValues
    where getCardValues = getValue . fromJust $ game^.cardInfo.tableCards

getValue :: [Card] -> Player -> Player
getValue cards' p = p & hand .~ hand'
                      & handValue .~ Just value'
    where allCards = cards' ++ p^.cards
          (value', hand') = bestHand allCards

{- this function will return the players who are tied for the best hands,
and the players who have lost. Callee will need to compare tied hands for 
tiebreakers. Once the values have been updated, will probably need to re-sort
the player list on num -}
getWinnersLosers :: Game -> ([Player], [Player])
getWinnersLosers game = span (((==) `on` (^.handValue)) $ head sorted') sorted'
    where sorted' = sortBy (flip sortOnHandValue) (game^.playerInfo.players)

sortOnHandValue :: Player -> Player -> Ordering
sortOnHandValue p1 p2 = compare (p1^.handValue) (p2^.handValue)

getWinners :: Game -> ([Player], [Player])
getWinners game = let results = getHandValue game
                      (topHands, loserHands) = getWinnersLosers results
                  in  getWinners' topHands loserHands

{- if any better than the head of list, filter them out, and retry. If none
better, filter any that are equal, and return. -}
getWinners' :: [Player] -> [Player] -> ([Player], [Player])
getWinners' [] _ = error "Potential winners can't be an empty list"
getWinners' topHands@(x:xs) loserHands
    | length topHands == 1 = (topHands, loserHands)
    | any greaterHandValue xs = getWinners' better (loserHands ++ worse)
    | otherwise = (best, loserHands ++ others)
    where greaterHandValue h = greaterHand h x
          equalHandValue h = equalHand h x
          (better, worse) = partition greaterHandValue topHands
          (best, others) = partition equalHandValue topHands

greaterHand :: Player -> Player -> Bool
greaterHand p1 p2 = handCompare (fromJust $ p1^.handValue, p1^.hand)
                                (fromJust $ p2^.handValue, p2^.hand)
                                 GT

equalHand :: Player -> Player -> Bool
equalHand p1 p2 = handCompare (fromJust $ p1^.handValue, p1^.hand)
                              (fromJust $ p2^.handValue, p2^.hand)
                               EQ

handCompare :: (Hand, [Card]) -> (Hand, [Card]) -> Ordering -> Bool
handCompare (StraightFlush, c1)
            (StraightFlush, c2) ordFunc = sortStraight c1 c2 == ordFunc

handCompare (FourOfAKind, c1)
            (FourOfAKind, c2) ordFunc = sortXOfAKind c1 c2 == ordFunc

handCompare (FullHouse, c1)
            (FullHouse, c2) ordFunc = sortXOfAKind c1 c2 == ordFunc

handCompare (Flush, c1)
            (Flush, c2) ordFunc = sortOnValue c1 c2 == ordFunc
                    
handCompare (Straight, c1)
            (Straight, c2) ordFunc = sortStraight c1 c2 == ordFunc

handCompare (ThreeOfAKind, c1)
            (ThreeOfAKind, c2) ordFunc = sortXOfAKind c1 c2 == ordFunc

handCompare (TwoPair, c1)
            (TwoPair, c2) ordFunc = sortXOfAKind c1 c2 == ordFunc

handCompare (Pair, c1)
            (Pair, c2) ordFunc = sortXOfAKind c1 c2 == ordFunc

handCompare (HighCard, c1)
            (HighCard, c2) ordFunc = sortOnValue c1 c2 == ordFunc

handCompare _ _ _ = error "Cards must be of same hand type"
