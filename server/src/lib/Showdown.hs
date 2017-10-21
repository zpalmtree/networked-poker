module Showdown
(
    distributePot,
    calculateHandValues
)
where

import Control.Lens
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Safe (headNote)
import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (get)
import Control.Monad (when)

import Types (GameState, Card, HandInfo, Player, Pot)
import Showdown.Ord (ordHand)
import Utilities.Player (leftOfDealer)

import Showdown.Value 
    (isStraightFlush7Card, isFourOfAKind, isFullHouse, isFlush, 
     isStraight7Card, isThreeOfAKind, isTwoPair, isPair)

import Showdown.Best 
    (bestStraightFlush, bestFourOfAKind, bestFullHouse, bestFlush, 
     bestStraight, bestThreeOfAKind, bestTwoPair, bestPair, bestHighCard)

import Lenses 
    (playerQueue, players, cardInfo, handInfo, tableCards, cards, uuid, 
     chips, pot, playerUUIDs)

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

calculateHandValues :: (Monad m) => GameState m ()
calculateHandValues = do
    s <- get
    let cards' = s^.cardInfo.tableCards

    zoom (playerQueue.players.traversed) $ do
        p <- get
        let allCards = cards' ++ p^.cards
        handInfo .= Just (topHand allCards)

getWinners :: [Player] -> [Player]
getWinners p = filter equalToWinner sorted
    where sorted = sortBy (flip sortHandValue) p
          winnerHand = fromJust $ headNote "in getWinners!" sorted^.handInfo
          equalToWinner x = ordHand winnerHand (fromJust (x^.handInfo)) == EQ

sortHandValue :: Player -> Player -> Ordering
sortHandValue p1 p2 = ordHand hand1 hand2
    where hand1 = fromJust $ p1^.handInfo
          hand2 = fromJust $ p2^.handInfo

distributePot :: (Monad m) => Pot -> GameState m [UUID]
distributePot sidePot = do
    s <- get

    let inPot = filter (\p -> p^.uuid `elem` sidePot^.playerUUIDs) 
                       (s^.playerQueue.players)
        winners = getWinners inPot
        chipsPerPerson = sidePot^.pot `div` length winners
        spareChips = sidePot^.pot `rem` length winners
        allPlayers = playerQueue.players.traversed
        isWinner p = p^.uuid `elem` winners^..traversed.uuid

    spareID <- leftOfDealer winners

    zoom (allPlayers.filtered isWinner) $ do
        p <- get

        chips += chipsPerPerson

        when (p^.uuid == spareID) $ chips += spareChips

    return $ winners^..traversed.uuid
