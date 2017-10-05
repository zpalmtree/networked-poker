{-# LANGUAGE TemplateHaskell #-}

module ShowdownTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Property (Property, (==>))
import Test.QuickCheck.Gen (shuffle)

import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execState)
import Data.Maybe (isJust)

import qualified Data.Poker as 
    Poker (Rank(..), Suit(..), CardSet, HandValue(..), fromList, mkCard, 
           handValue_n)

import Types (Game, Stage(..), Card(..), Hand(..), Value(..), Suit(..))
import Showdown (distributePot, topHand, calculateHandValues)

import Lenses 
    (pot, bet, chips, bets, pots, playerQueue, players, stage, handInfo,
     handValue, value, suit)

cardConvert :: [Card] -> Poker.CardSet
cardConvert cards = Poker.fromList converted
    where converted = map convert cards
          convert card = Poker.mkCard (convertRank $ card^.value) 
                                      (convertSuit $ card^.suit)

          convertRank :: Value -> Poker.Rank
          convertRank Two = Poker.Two
          convertRank Three = Poker.Three
          convertRank Four = Poker.Four
          convertRank Five = Poker.Five
          convertRank Six = Poker.Six
          convertRank Seven = Poker.Seven
          convertRank Eight = Poker.Eight
          convertRank Nine = Poker.Nine
          convertRank Ten = Poker.Ten
          convertRank Jack = Poker.Jack
          convertRank Queen = Poker.Queen
          convertRank King = Poker.King
          convertRank Ace = Poker.Ace

          convertSuit :: Suit -> Poker.Suit
          convertSuit Heart = Poker.Hearts
          convertSuit Diamond = Poker.Diamonds
          convertSuit Club = Poker.Clubs
          convertSuit Spade = Poker.Spades

equalHand :: Hand Value Value -> Poker.HandValue -> Bool
equalHand HighCard{}        Poker.NoPair{}          = True
equalHand Pair{}            Poker.OnePair{}         = True
equalHand TwoPair{}         Poker.TwoPair{}         = True
equalHand ThreeOfAKind{}    Poker.ThreeOfAKind{}    = True
equalHand Straight{}        Poker.Straight{}        = True
equalHand Flush{}           Poker.Flush{}           = True
equalHand FullHouse{}       Poker.FullHouse{}       = True
equalHand FourOfAKind{}     Poker.FourOfAKind{}     = True
equalHand StraightFlush{}   Poker.StraightFlush{}   = True
equalHand _ _ = False

newtype Unique7CardHand a = Unique7CardHand { 
    getHand :: [Card]
} deriving (Show)

instance Arbitrary (Unique7CardHand a) where
    arbitrary = do
        shuffled <- shuffle fullDeck
        return . Unique7CardHand $ take 7 shuffled
        where fullDeck = [Card value' suit' | 
                               value' <- [minBound :: Value .. maxBound],
                               suit' <- [minBound :: Suit .. maxBound]]


prop_topHandCorrect :: Unique7CardHand a -> Bool
prop_topHandCorrect cards = equalHand result knownResult
    where result = topHand (getHand cards)^.handValue
          knownResult = Poker.handValue_n 7 . cardConvert $ getHand cards

prop_calculateHandValuesSet :: Game -> Property
prop_calculateHandValuesSet s
    = valid ==> all isJust (once^..playerQueue.players.traversed.handInfo)

    where valid = s^.stage `elem` [River, Showdown]
          -- need to have all table cards to calculate hand values
          once = execState calculateHandValues s
                
prop_distributePotChipsPreserved :: Game -> Property
prop_distributePotChipsPreserved s 
    = valid ==> test

    where start = allChips s
          end = allChips $ execState f s
          pot' = head $ s^.bets.pots
          test = start + pot'^.pot == end
          f = do
            calculateHandValues
            distributePot pot'
          valid = s^.stage `elem` [River, Showdown]

allChips :: Game -> Int
allChips lens = sum $ map sum [lens^..allPlayers.bet,
                               lens^..allPlayers.chips,
                               lens^..bets.pots.traversed.pot]
    where allPlayers = playerQueue.players.traversed

--this block needs to be at the bottom of the file apparently
return []
runTests :: IO Bool
runTests = $quickCheckAll
