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
    GameState
)
where

import Control.Monad (replicateM, filterM)
import Control.Monad.Trans.State (StateT(..), State)
import Data.Char (toLower)
import Control.Lens (Lens', (^..), (^.), traversed, lens)
import Data.UUID.Types (UUID, fromWords)
import GHC.Word (Word32)
import System.Random (Random(..))

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Test.QuickCheck.Modifiers 
    (NonNegative(..), Positive(..))

import Test.QuickCheck.Gen 
    (Gen(..), choose, shuffle, suchThat, elements, resize, listOf1)

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
} deriving (Show, Eq)

data Player = Player {
    _name :: String,
    _uuid :: UUID,
    _chips :: Int,
    _cards :: [Card],
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
} deriving (Show, Eq)

data Bets = Bets {
    _pots :: [Pot],
    _currentBet :: Int,
    _smallBlindSize :: Int,
    _bigBlindSize :: Int,
    _minimumRaise :: Int
} deriving (Show, Eq)

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
    _playerUUIDs :: [UUID]
} deriving (Show, Eq)

data PlayerQueue = PlayerQueue {
    _players :: [Player],
    _dealer :: Int
} deriving (Show, Eq)

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

-- LENSES FOR EASIER INSTANCE DECLARATION

-- manually deriving lenses because deriving them before the data
-- types doesn't work, and after the data types cause scope errors
players :: Lens' PlayerQueue [Player]
players = lens _players (\queue v -> queue { _players = v })

cards :: Lens' Player [Card]
cards = lens _cards (\p v -> p { _cards = v })

uuid :: Lens' Player UUID
uuid = lens _uuid (\p v -> p { _uuid = v })

allIn :: Lens' Player Bool
allIn = lens _allIn (\p v -> p { _allIn = v})

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
        stage' <- arbitrary
        queue' <- arbitraryQueue stage'
        cardInfo' <- arbitraryCardInfo queue' stage'
        roundDone' <- arbitrary
        bets' <- arbitraryBets queue'
        gameFinished' <- arbitrary
        roundNumber' <- arbitrary
        return $ Game queue' stage' cardInfo' roundDone' bets' gameFinished'
                      roundNumber'

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Stage where
    arbitrary = elements [PreFlop, Flop, Turn, River, Showdown]

instance Arbitrary Suit where
    arbitrary = elements [Heart, Spade, Club, Diamond]

instance Arbitrary Value where
    arbitrary = elements [Two, Three, Four, Five, Six, Seven, Eight, Nine,
                          Ten, Jack, Queen, King, Ace]

instance Arbitrary UUID where
    arbitrary = uuidFromWords <$> arbitrary

instance Random Value where
    random g = case randomR (fromEnum (minBound :: Value),
                             fromEnum (maxBound :: Value)) g of
                    (r, g') -> (toEnum r, g')

    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID
uuidFromWords (a, b, c, d) = fromWords a b c d

arbitraryPot :: [UUID] -> Gen Pot
arbitraryPot ids = do
    (NonNegative pot') <- arbitrary
    actualIds' <- sublistOf1 ids
    return $ Pot pot' actualIds'

sublistOf1 :: [a] -> Gen [a]
sublistOf1 [] = return []
sublistOf1 (x:xs) = do
    result <- filterM (\_ -> choose (False, True)) xs
    return (x : result)

arbitraryBets :: PlayerQueue -> Gen Bets
arbitraryBets queue' = do
    Positive n <- arbitrary
    pots' <- replicateM (if anyAllIn then n else 1) (arbitraryPot ids)
    (NonNegative currentBet') <- arbitrary

    --this could become too big and cause problems, might need to constrain
    --on chips somehow or just force small values
    (NonNegative smallBlindSize') <- arbitrary
    let bigBlindSize' = smallBlindSize' * 2

    --minimumRaise much be at least big blind
    minimumRaise' <- suchThat arbitrary (> bigBlindSize')

    return $ Bets pots' currentBet' smallBlindSize' bigBlindSize'
                    minimumRaise'
    where ids = queue'^..players.traversed.uuid
          anyAllIn = any (^.allIn) (queue'^..players.traversed)

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

arbitraryPlayers :: Stage -> Gen Player
arbitraryPlayers stage = do
    name' <- arbitrary
    uuid' <- arbitrary -- what happens if this is negative?
    (NonNegative chips') <- arbitrary
    cards' <- if stage == PreFlop then return [] else cardGen
    inPlay' <- arbitrary
    allIn' <- arbitrary
    (NonNegative bet') <- arbitrary
    madeInitialBet' <- arbitrary

    --we can't set this up, because to get valid values we need to know
    --the cards on the table. We can't know this, because to get the valid
    --cards on the table, we need to know the players cards
    let handInfo' = Nothing
    canReRaise' <- arbitrary
    return $ Player name' uuid' chips' cards' inPlay' allIn' bet'
                    madeInitialBet' handInfo' canReRaise'
    where cardGen = replicateM 2 arbitrary

-- set size param, use list of to cap
arbitraryQueue :: Stage -> Gen PlayerQueue
arbitraryQueue stage = do
    players' <- resize 23 . listOf1 $ arbitraryPlayers stage
    dealer' <- choose (0, length players' - 1)
    return $ PlayerQueue players' dealer'
