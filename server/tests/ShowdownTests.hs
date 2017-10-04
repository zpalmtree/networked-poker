{-# LANGUAGE TemplateHaskell #-}

module ShowdownTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Property (Property, (==>))
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, choose, sample', shuffle)
import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execState)
import Control.Monad (replicateM, when)
import Data.Maybe (isJust)
import Data.List (sort)

import Types (Game, Stage(..), Card(..), Hand(..), Value(..), Suit(..))
import Showdown (distributePot, topHand, calculateHandValues)

import Lenses 
    (pot, bet, chips, bets, pots, playerQueue, players, stage, handInfo,
     handValue)

newtype StraightFlush' a = StraightFlush' { getCardsSF :: [Card] } deriving (Show)
newtype FourOfAKind' a = FourOfAKind' { getCards4 :: [Card] } deriving (Show)
newtype FullHouse' a = FullHouse' { getCardsFH :: [Card] } deriving (Show)
newtype Flush' a = Flush' { getCardsF :: [Card] } deriving (Show)
newtype Straight' a = Straight' { getCardsS :: [Card] } deriving (Show)
newtype ThreeOfAKind' a = ThreeOfAKind' { getCards3 :: [Card] } deriving (Show)
newtype TwoPair' a = TwoPair' { getCards2 :: [Card] } deriving (Show)
newtype Pair' a = Pair' { getCards1 :: [Card] } deriving (Show)
newtype HighCard' a = HighCard' { getCardsH :: [Card] } deriving (Show)

instance Arbitrary (StraightFlush' a) where
    arbitrary = do
        suit <- arbitrary

        -- generate initial value between two and ten
        initialValue <- choose (Two, Ten) 

        --make the straight from value1 upwards
        let values = take 5 [initialValue .. ]
            cards = map (\value -> Card value suit) (take 5 [initialValue .. ])

        -- remove the generated cards from full deck so we don't get
        -- duplication for the final two values
        let remaining = removeFrom fullDeck cards

        shuffled <- shuffle remaining

        let final2 = take 2 shuffled

        return . StraightFlush' $ cards ++ final2

instance Arbitrary (FourOfAKind' a) where
    arbitrary = do
        value <- arbitrary

        let cards = map (\suit -> Card value suit) [Heart .. Diamond]
            remaining = removeFrom fullDeck cards

        shuffled <- shuffle remaining

        let final3 = take 3 shuffled

        return . FourOfAKind' $ cards ++ final3

instance Arbitrary (FullHouse' a) where
    arbitrary = do
        value <- arbitrary

        let remaining = removeFrom fullValues [value]

        shuffled <- shuffle remaining

        let value' = head $ shuffled

        suits <- shuffle allSuits
        suits' <- shuffle allSuits

        let twos = map (\suit -> Card value suit) (take 2 suits)
            threes = map (\suit -> Card value' suit) (take 3 suits')
            fh = twos ++ threes

            --remove the remaining card of the three of a kind, and one of 
            --the remaining two of a kind so we can't accidentaly get a 
            --four of a kind 
            missingThree = head $ removeFrom 
                                  (map (\suit -> Card value' suit) allSuits) 
                                  threes

            oneOfTwos = head $ removeFrom 
                               (map (\suit -> Card value suit) allSuits) 
                               twos

            fixedDeck = removeFrom fullDeck [missingThree, oneOfTwos]
            remaining' = removeFrom fixedDeck fh

        shuffled' <- shuffle remaining'

        let final2 = take 2 shuffled'

        return . FullHouse' $ fh ++ final2

        where fullValues = [Two .. Ace]
              allSuits = [Heart .. Diamond]

instance Arbitrary (Flush' a) where
    arbitrary = do
        suit <- arbitrary

        shuffled <- shuffle fullValues

        let values = take 5 shuffled -- these could rarely be a straight flush
            cards = map (\value -> Card value suit) values
            remaining = removeFrom fullDeck cards
            sorted = sort values
            generated = [head sorted .. last sorted]

        -- if the lowest value to the highest value gives a length of 5
        -- the cards are a straight flush, so just call the method again
        -- it's a bit lazy but it's pretty unlikely so it's a simple way to 
        -- fix the issue
        when (length generated == 5) $ arbitrary

        shuffled <- shuffle remaining

        let final2 = take 2 shuffled

        return . Flush' $ cards ++ final2

        where fullValues = [Two .. Ace]

removeFrom :: (Eq a) => [a] -> [a] -> [a]
removeFrom big small = filter (`notElem` small) big

fullDeck :: [Card]
fullDeck = [Card value suit | 
                 value <- [minBound :: Value .. maxBound],
                 suit <- [minBound :: Suit .. maxBound]]


prop_topHandStraightFlush :: StraightFlush' a -> Bool
prop_topHandStraightFlush sf = isStraightFlush' hand
    where hand = (topHand $ getCardsSF sf)^.handValue
          isStraightFlush' (StraightFlush _ _) = True
          isStraightFlush' _ = False

prop_topHandFourOfAKind :: FourOfAKind' a -> Bool
prop_topHandFourOfAKind f = isFourOfAKind' hand
    where hand = (topHand $ getCards4 f)^.handValue
          isFourOfAKind' (FourOfAKind _) = True
          isFourOfAKind' _ = False

prop_topHandFullHouse :: FullHouse' a -> Bool
prop_topHandFullHouse fh = isFullHouse' hand
    where hand = (topHand $ getCardsFH fh)^.handValue
          isFullHouse' (FullHouse _ _) = True
          isFullHouse' _ = False

prop_topHandFlush :: Flush' a -> Bool
prop_topHandFlush f = isFlush' hand
    where hand = (topHand $ getCardsF f)^.handValue
          isFlush' (Flush _) = True
          isFlush' _ = False

prop_topHandStraight :: Straight' a -> Bool
prop_topHandStraight s = isStraight' hand
    where hand = (topHand $ getCardsS s)^.handValue
          isStraight' (Straight _ _) = True
          isStraight' _ = False

prop_topHandThreeOfAKind :: ThreeOfAKind' a -> Bool
prop_topHandThreeOfAKind t = isThreeOfAKind' hand
    where hand = (topHand $ getCards3 t)^.handValue
          isThreeOfAKind' (ThreeOfAKind _) = True
          isThreeOfAKind' _ = False

prop_topHandTwoPair :: TwoPair' a -> Bool
prop_topHandTwoPair t = isTwoPair' hand
    where hand = (topHand $ getCards2 t)^.handValue
          isTwoPair' (TwoPair _ _) = True
          isTwoPair' _ = False

prop_topHandPair :: Pair' a -> Bool
prop_topHandPair p = isPair' hand
    where hand = (topHand $ getCards1 p)^.handValue
          isPair' (Pair _) = True
          isPair' _ = False

prop_topHandHighCard :: HighCard' a -> Bool
prop_topHandHighCard h = isHighCard' hand
    where hand = (topHand $ getCardsH h)^.handValue
          isHighCard' (HighCard _) = True
          isHighCard' _ = False

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
