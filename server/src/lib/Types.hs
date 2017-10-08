{-# LANGUAGE DeriveGeneric #-}

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
    ActionMessage(..),
    GameStateT,
    GameState
)
where

import Control.Monad.Trans.State (StateT(..), State)
import Data.Char (toLower)
import Data.UUID.Types (UUID)
import Control.Concurrent.MVar (MVar)
import Network.Socket (Socket)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Text.Printf 
    (PrintfArg(..), printf, fmtPrecision, fmtChar, vFmt, formatString, 
     errorBadFormat)

-- DATA TYPES

data Game = Game {
    _playerChan :: MVar [Player],
    _playerQueue :: PlayerQueue,
    _stage :: Stage,
    _cardInfo :: Cards,
    _roundDone :: Bool,
    _bets :: Bets,
    _gameFinished :: Bool,
    _roundNumber :: Int
}

data Player = Player {
    _socket :: Socket,
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
              | Raise Int
              | AllIn 
              deriving (Generic, Show)

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

data ActionMessage a = ActionMessage {
    _action :: Action a,
    _player :: UUID
} deriving (Generic)

-- TYPES

type GameStateT a = StateT Game IO a

type GameState a = State Game a

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

instance Binary (ActionMessage a)

instance Binary (Action a)
