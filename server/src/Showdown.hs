module Showdown
(
    distributePot,
    getHandValue
)
where

import Types
import Showdown.Value
import Showdown.Best
import Showdown.Ord
import PlayerUtilities
import Lenses (playerInfo, players, cardInfo, handInfo, tableCards, cards, num,
               playerIDs, chips, pot)

import Control.Lens
import Data.Maybe
import Data.List

topHand :: [Card] -> HandInfo
topHand cards'
    | isStraightFlush7Card cards' = bestStraightFlush cards'
    | isFourOfAKind cards' = bestFourOfAKind cards'
    | isFullHouse cards' = bestFullHouse cards'
    | isFlush cards' = bestFlush cards'
    | isStraight7Card cards' = bestStraight cards'
    | isThreeOfAKind cards' = bestThreeOfAKind cards'
    | isTwoPair cards' = bestTwoPair cards'
    | isPair cards' = bestPair cards'
    | otherwise = bestHighCard cards'

getHandValue :: Game -> Game
getHandValue game = game & playerInfo.players.traversed %~ getCardValues
    where getCardValues = getValue $ game^.cardInfo.tableCards

getValue :: [Card] -> Player -> Player
getValue cards' p = p & handInfo .~ Just info
    where allCards = cards' ++ p^.cards
          info = topHand allCards

getWinnersLosers :: [Player] -> ([Player], [Player])
getWinnersLosers p = span equalToWinner sorted
    where sorted = sortBy (flip sortHandValue) p
          winnerHand = fromJust $ head sorted^.handInfo
          equalToWinner x = ordHand winnerHand (fromJust (x^.handInfo)) == EQ

sortHandValue :: Player -> Player -> Ordering
sortHandValue p1 p2 = ordHand hand1 hand2
    where hand1 = fromJust $ p1^.handInfo
          hand2 = fromJust $ p2^.handInfo

distributePot :: Game -> Pot -> (Game, (Pot, [Player]))
distributePot game sidePot = (newGame, winnerMapping)
    where people = filter isInPot (game^.playerInfo.players)
          isInPot p = p^.num `elem` sidePot^.playerIDs          
          (winners, _) = getWinnersLosers people
          (spareRecipient, rest) = leftOfDealer game winners 1
          newGame = giveWinningsSplitPot game sidePot (spareRecipient, rest)
          winnerMapping = (sidePot, winners)

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
