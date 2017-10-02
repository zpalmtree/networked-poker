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
import Control.Monad (replicateM)
import Data.Maybe (isJust)

import Types (Game, Stage(..), Card(..), Hand(..), Value(..), Suit(..))
import Showdown (distributePot, topHand, calculateHandValues)

import Lenses 
    (pot, bet, chips, bets, pots, playerQueue, players, stage, handInfo,
     handValue)

newtype StraightFlush' a = StraightFlush' { getCardsSF :: [Card] }
newtype FourOfAKind' a = FourOfAKind' { getCards4 :: [Card] }
newtype FullHouseHand' a = FullHouseHand' { getCardsFH :: [Card] }
newtype Flush' a = Flush' { getCardsF :: [Card] }
newtype Straight' a = Straight' { getCardsS :: [Card] }
newtype ThreeOfAKind' a = ThreeOfAKind' { getCards3 :: [Card] }
newtype TwoPair' a = TwoPair' { getCards2 :: [Card] }
newtype Pair' a = Pair' { getCards1 :: [Card] }
newtype HighCard' a = HighCard' { getCardsH :: [Card] }

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

instance Show (StraightFlush' a) where
    show x = show $ getCardsSF x

outputStraightFlush :: IO ()
outputStraightFlush = output (arbitrary :: Gen (StraightFlush' a)) getCardsSF

--checkCards :: Gen 
output :: Show a => Gen t -> (t -> a) -> IO ()
output gen getter = do
    results <- sample' gen
    mapM_ (\x -> print $ getter x) results

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

{-
prop_topHandFourOfAKind :: [Card] -> Property
prop_topHandFourOfAKind cards' = length cards' == 7 && isFourOfAKind ==>
    isFourOfAKind' $ hand^.handValue
    where hand = topHand cards'
          isFourOfAKind' (FourOfAKind _) = True
          isFourOfAKind' _ = False

prop_topHandFullHouse :: [Card] -> Property
prop_topHandFullHouse cards' = length cards' == 7 && isFullHouse ==>
    isFullHouse' $ hand^.handValue
    where hand = topHand cards'
          isFullHouse' (FullHouse _ _) = True
          isFullHouse' _ = False

prop_topHandFlush :: [Card] -> Property
prop_topHandFlush cards' = length cards' == 7 && isFlush ==>
    isFlush' $ hand^.handValue
    where hand = topHand cards'
          isFlush' (Flush _) = True
          isFlush' _ = False

prop_topHandStraight :: [Card] -> Property
prop_topHandStraight cards' = length cards' == 7 && isStraight ==>
    isStraight' $ hand^.handValue
    where hand = topHand cards'
          isStraight' (Straight _ _) = True
          isStraight' _ = False

prop_topHandThreeOfAKind :: [Card] -> Property
prop_topHandThreeOfAKind cards' = length cards' == 7 && isThreeOfAKind ==>
    isThreeOfAKind' $ hand^.handValue
    where hand = topHand cards'
          isThreeOfAKind' (ThreeOfAKind _) = True
          isThreeOfAKind' _ = False

prop_topHandTwoPair :: [Card] -> Property
prop_topHandTwoPair cards' = length cards' == 7 && isTwoPair ==>
    isTwoPair' $ hand^.handValue
    where hand = topHand cards'
          isTwoPair' (TwoPair _ _) = True
          isTwoPair' _ = False

prop_topHandHighCard :: [Card] -> Property
prop_topHandHighCard cards' = length cards' == 7 && isHighCard ==>
    isHighCard' $ hand^.handValue
    where hand = topHand cards'
          isHighCard' (HighCard _) = True
          isHighCard' _ = False

isStraightFlush = undefined
isFourOfAKind = undefined
isFullHouse = undefined
isFlush = undefined
isStraight = undefined
isThreeOfAKind = undefined
isTwoPair = undefined
isHighCard = undefined
-}

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
