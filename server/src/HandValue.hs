module HandValue
(
    bestHand,
    getWinnersLosers,
    getHandValue
)
where

import Types
import HandValue.Value
import HandValue.Best
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
getValue cards' p = p & hand .~ Just hand'
                      & handValue .~ Just value'
    where allCards = cards' ++ fromJust (p^.cards)
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
