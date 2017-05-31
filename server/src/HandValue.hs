module HandValue
(
    bestHand
)
where

import Types
import HandValue.Value
import HandValue.Best

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
