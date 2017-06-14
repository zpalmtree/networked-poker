module Showdown
(
    distributePot,
    getHandValue
)
where

import Types
import Showdown.Value
import Showdown.Best
import Showdown.Sort
import PlayerUtilities

import Control.Lens
import Data.Maybe
import Data.List
import Data.Function

bestHand :: [Card] -> (Hand, [Card])
bestHand cards'
    | isStraightFlush7Card cards' = (StraightFlush, bestStraightFlush cards')
    | isFourOfAKind cards' =        (FourOfAKind,   bestFourOfAKind cards')
    | isFullHouse cards' =          (FullHouse,     bestFullHouse cards')
    | isFlush cards' =              (Flush,         bestFlush cards')
    | isStraight7Card cards' =      (Straight,      bestStraight cards')
    | isThreeOfAKind cards' =       (ThreeOfAKind,  bestThreeOfAKind cards')
    | isTwoPair cards' =            (TwoPair,       bestTwoPair cards')
    | isPair cards' =               (Pair,          bestPair cards')
    | otherwise =                   (HighCard,      bestHighCard cards')

getHandValue :: Game -> Game
getHandValue game = game & playerInfo.players.traversed %~ getCardValues
    where getCardValues = getValue $ game^.cardInfo.tableCards

getValue :: [Card] -> Player -> Player
getValue cards' p = p & hand .~ hand'
                      & handValue .~ Just value'
    where allCards = cards' ++ p^.cards
          (value', hand') = bestHand allCards

-- this function will return the players who are tied for the best hands,
-- and the players who have lost. Callee will need to compare tied hands for 
-- tiebreakers. Once the values have been updated, will probably need to
-- re-sort the player list on num
getWinnersLosers :: [Player] -> ([Player], [Player])
getWinnersLosers p = span (((==) `on` (^.handValue)) $ head sorted') sorted'
    where sorted' = sortBy (flip sortOnShowdown) p

sortOnShowdown :: Player -> Player -> Ordering
sortOnShowdown p1 p2 = compare (p1^.handValue) (p2^.handValue)

distributePot :: Game -> Pot -> (Game, (Pot, [Player]))
distributePot game sidePot = (newGame, winnerMapping)
    where people = filter isInPot (game^.playerInfo.players)
          isInPot p = p^.num `elem` sidePot^.playerIDs          
          (topHands, loserHands) = getWinnersLosers people
          (winners, _) = getWinners' topHands loserHands
          (spareRecipient, rest) = leftOfDealer game winners 1
          newGame = giveWinningsSplitPot game sidePot (spareRecipient, rest)
          winnerMapping = (sidePot, winners)

-- if any better than the head of list, filter them out, and retry. If none
-- better, filter any that are equal, and return.
getWinners' :: [Player] -> [Player] -> ([Player], [Player])
getWinners' [] _ = error "Potential winners can't be an empty list"
getWinners' topHands@(x:xs) loserHands
    | length topHands == 1 = (topHands, loserHands)
    | any greaterShowdown xs = getWinners' better (loserHands ++ worse)
    | otherwise = (best, loserHands ++ others)
    where greaterShowdown h = greaterHand h x
          equalShowdown h = equalHand h x
          (better, worse) = partition greaterShowdown topHands
          (best, others) = partition equalShowdown topHands

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

giveWinningsSplitPot :: Game -> Pot -> (Player, [Player]) -> Game
giveWinningsSplitPot game sidePot (spare, rest) = newGame
    where newPlayers = map (addChips ids (spare^.num) sidePot) p
          ids = map (^.num) rest
          p = game^.playerInfo.players
          newGame = game & playerInfo.players .~ newPlayers

addChips :: [Int] -> Int -> Pot -> Player -> Player
addChips ids spareId sidePot p
    | p^.num == spareId = p & chips +~ chipsPerPerson + spareChips
    | p^.num `elem` ids = p & chips +~ chipsPerPerson
    | otherwise = p
    where chips' = sidePot^.pot
          chipsPerPerson = chips' `div` (length ids + 1)
          spareChips = chips' `rem` (length ids + 1)
