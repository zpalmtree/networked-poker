module Types
(
    Game(..),
    Player(..),
    Cards(..),
    Bets(..),
    Card(..),
    HandInfo(..),
    Pot(..),
    PlayerQueue(..),
    Stage(..),
    Suit(..),
    Value(..),
    Action(..),
    Hand(..),
    GameStateT,
    GameState,
    PlayerID
)
where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Modifiers (NonEmptyList(..), NonNegative(..))
import Test.QuickCheck.Gen 
    (Gen(..), choose, shuffle, suchThat, oneof, sublistOf, elements)

import Control.Monad (replicateM)
import Control.Monad.Trans.State (StateT(..), State)
import Data.Char (toLower)
import Control.Lens (Lens', (^..), traversed, lens)

import Text.Printf 
    (PrintfArg(..), printf, fmtPrecision, fmtChar, vFmt, formatString, 
     errorBadFormat)

-- DATA TYPES

data Game = Game {
    _playerQueue :: PlayerQueue,
    _stage :: Stage,
    _cardInfo :: Cards,
    _roundDone :: Bool,
    _bets :: Bets,
    _gameFinished :: Bool,
    _roundNumber :: Int
} deriving (Show)

data Player = Player {
    _name :: String,
    _num :: PlayerID,
    _chips :: Int,
    _cards :: [Card], -- why do i have this
    _inPlay :: Bool,
    _allIn :: Bool,
    _bet :: Int,
    _madeInitialBet :: Bool,
    _handInfo :: Maybe HandInfo,
    _canReRaise :: Bool
} deriving (Eq, Show)

data Cards = Cards {
    _tableCards :: [Card],
    _deck :: [Card]
} deriving (Show)

data Bets = Bets {
    _pots :: [Pot],
    _currentBet :: Int,
    _smallBlindSize :: Int,
    _bigBlindSize :: Int,
    _minimumRaise :: Int
} deriving (Show)

data Card = Card {
    _value :: Value,
    _suit :: Suit
} deriving (Eq)

data HandInfo = HandInfo {
    _handValue :: Hand Value Value,
    _bestHand :: [Card]
} deriving (Eq, Show)

data Pot = Pot {
    _pot :: Int,
    _playerIDs :: [PlayerID]
} deriving (Show)

data PlayerQueue = PlayerQueue {
    _players :: [Player],
    _dealer :: Int
} deriving (Show)

data Stage = PreFlop 
           | Flop 
           | Turn 
           | River 
           | Showdown 
           deriving (Show, Eq)

data Suit = Heart 
          | Spade 
          | Club 
          | Diamond 
          deriving (Show, Bounded, Enum, Eq)

data Value = Two 
           | Three 
           | Four 
           | Five 
           | Six 
           | Seven 
           | Eight 
           | Nine 
           | Ten 
           | Jack 
           | Queen 
           | King 
           | Ace 
           deriving (Show, Bounded, Enum, Eq, Ord)

data Action a = Fold 
              | Check 
              | Call 
              | Raise a 
              | AllIn 
              deriving (Show)

data Hand a b = HighCard a 
              | Pair a 
              | TwoPair a b
              | ThreeOfAKind a
              | Straight a b 
              | Flush a
              | FullHouse a b 
              | FourOfAKind a
              | StraightFlush a b
              deriving (Eq, Ord)

-- TYPES

type GameStateT a = StateT Game IO a

type GameState a = State Game a

type PlayerID = Int

-- LENSES FOR EASIER INSTANCE DECLARATION

-- manually deriving lenses because deriving them before the data
-- types doesn't work, and after the data types cause scope errors
players :: Lens' PlayerQueue [Player]
players = lens _players (\queue v -> queue { _players = v })

cards :: Lens' Player [Card]
cards = lens _cards (\p v -> p { _cards = v })
--why doesn't this work?
num :: Lens' Player PlayerID
num = lens _num (\p v -> p { _num = v })

-- INSTANCES

instance (PrintfArg a, PrintfArg b) => Show (Hand a b) where
    show (HighCard a)
        = printf "high card %V" a

    show (Pair a)
        = printf "pair of %Us" a

    show (TwoPair a b)
        = printf "two pair, %Us and %Us" a b

    show (ThreeOfAKind a)
        = printf "three of a kind, %Us" a

    show (Straight a b)
        = printf "straight, %V to %V" a b

    show (Flush a)
        = printf "flush, %V high" a

    show (FullHouse a b)
        = printf "full house, %Us over %Us" a b

    show (FourOfAKind a)
        = printf "four of a kind, %Us" a

    show (StraightFlush a b)
        = printf "straight flush, %V to %V" a b

instance PrintfArg Value where
    formatArg x fmt
        | fmtChar (vFmt 'V' fmt) == 'V' 
            = formatString (map toLower $ show x) 
              (fmt { fmtChar = 's', fmtPrecision = Nothing })

        | fmtChar (vFmt 'U' fmt) == 'U'
            = formatString (plural . map toLower $ show x)
              (fmt { fmtChar = 's', fmtPrecision = Nothing})

        | otherwise = errorBadFormat $ fmtChar fmt
        where plural "six" = "sixe"
              plural s = s

instance Show Card where
    show (Card value' suit') = show value' ++ " of " ++ show suit' ++ "s"

instance Arbitrary Game where
    arbitrary = do
        queue' <- arbitrary
        stage' <- arbitrary
        cardInfo' <- arbitraryCardInfo queue' stage'
        roundDone' <- arbitrary
        let playerIDs = queue'^..players.traversed.num
        bets' <- arbitraryBets playerIDs
        gameFinished' <- arbitrary
        roundNumber' <- arbitrary
        return $ Game queue' stage' cardInfo' roundDone' bets' gameFinished'
                      roundNumber'

instance Arbitrary PlayerQueue where
    arbitrary = do
        (NonEmpty players') <- arbitrary
        dealer' <- choose (0, length players' - 1)
        return $ PlayerQueue players' dealer'

instance Arbitrary Player where
    arbitrary = do
        name' <- arbitrary
        num' <- arbitrary -- what happens if this is negative?
        (NonNegative chips') <- arbitrary
        cards' <- oneof [return [], cardGen]
        inPlay' <- arbitrary
        allIn' <- arbitrary
        (NonNegative bet') <- arbitrary
        madeInitialBet' <- arbitrary

        --we can't set this up, because to get valid values we need to know
        --the cards on the table. We can't know this, because to get the valid
        --cards on the table, we need to know the players cards
        let handInfo' = Nothing
        canReRaise' <- arbitrary
        return $ Player name' num' chips' cards' inPlay' allIn' bet'
                        madeInitialBet' handInfo' canReRaise'
        where cardGen = replicateM 2 arbitrary

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Stage where
    arbitrary = elements [PreFlop, Flop, Turn, River, Showdown]

instance Arbitrary Suit where
    arbitrary = elements [Heart, Spade, Club, Diamond]

instance Arbitrary Value where
    arbitrary = elements [Two, Three, Four, Five, Six, Seven, Eight, Nine,
                          Ten, Jack, Queen, King, Ace]

arbitraryPot :: [PlayerID] -> Gen Pot
arbitraryPot ids = do
    (NonNegative pot') <- arbitrary
    actualIds' <- sublistOf ids
    return $ Pot pot' actualIds'

arbitraryBets :: [PlayerID] -> Gen Bets
arbitraryBets ids = do
    n <- arbitrary
    pots' <- replicateM n (arbitraryPot ids)
    (NonNegative currentBet') <- arbitrary

    --this could become too big and cause problems, might need to constrain
    --on chips somehow or just force small values
    (NonNegative smallBlindSize') <- arbitrary
    let bigBlindSize' = smallBlindSize' * 2

    --minimumRaise much be at least big blind
    minimumRaise' <- suchThat arbitrary (> bigBlindSize')

    return $ Bets pots' currentBet' smallBlindSize' bigBlindSize'
                    minimumRaise'

drawN :: (Foldable t) => Int -> t a -> Gen ([Card], [Card])
drawN n playerCards
    | n > (length fullDeck - length playerCards)
      = error "Can't draw that many cards!"
    | otherwise = do
        shuffled <- shuffle fullDeck  
        return $ splitAt n shuffled
    where fullDeck = [Card value suit | 
                           value <- [minBound :: Value .. maxBound],
                           suit <- [minBound :: Suit .. maxBound]]

arbitraryCardInfo :: PlayerQueue -> Stage -> Gen Cards
arbitraryCardInfo pq s = case s of
    PreFlop -> takeN 0
    Flop -> takeN 3
    Turn -> takeN 4
    River -> takeN 5
    Showdown -> takeN 5

    where playerCards = concat $ pq^..players.traversed.cards
          takeN n = do
            (tableCards', deck') <- drawN n playerCards
            return $ Cards tableCards' deck'
