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
    Message(..),
    ActionMessage(..),
    PlayerTurnMessage(..),
    CardMessage(..),
    DealtCardsMessage(..),
    PotWinnersMessage(..),
    GameOverMessage(..),
    PlayersRemovedMessage(..),
    CardRevealMessage(..),
    PlayerHandInfo(..),
    GameStateT,
    GameState
)
where

import Control.Monad.Trans.State (StateT(..), State)
import Data.UUID.Types (UUID)
import Control.Concurrent.MVar (MVar)
import Network.Socket (Socket)
import Data.Binary (Binary)
import GHC.Generics (Generic)

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
} deriving (Eq)

data Cards = Cards {
    _tableCards :: [Card],
    _deck :: [Card]
} deriving (Eq)

data Bets = Bets {
    _pots :: [Pot],
    _currentBet :: Int,
    _smallBlindSize :: Int,
    _bigBlindSize :: Int,
    _minimumRaise :: Int
} deriving (Eq)

data Card = Card {
    _value :: Value,
    _suit :: Suit
} deriving (Eq, Generic)

data HandInfo = HandInfo {
    _handValue :: Hand Value Value,
    _bestHand :: [Card]
} deriving (Eq)

data Pot = Pot {
    _pot :: Int,
    _playerUUIDs :: [UUID]
} deriving (Eq, Generic)

data PlayerQueue = PlayerQueue {
    _players :: [Player],
    _dealer :: Int
} deriving (Eq)

data ActionMessage a = ActionMessage {
    _action :: Action a,
    _player :: UUID
} deriving (Generic)

data PlayerHandInfo = PlayerHandInfo {
    _id :: UUID,
    _rank :: Hand Value Value,
    _hand :: [Card]
} deriving (Generic)

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
          deriving (Show, Bounded, Enum, Eq, Generic)

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
           deriving (Show, Bounded, Enum, Eq, Ord, Generic)

data Action a = Fold 
              | Check 
              | Call 
              | Raise Int
              | AllIn 
              | SmallBlind
              | BigBlind
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
              deriving (Eq, Ord, Generic)

data GameOverMessage = GameOverMessage deriving (Generic)

-- NEWTYPES

newtype Message a = Message { 
    getMessage :: a
} deriving (Generic)

newtype CardMessage = CardMessage {
    _allCards :: [Card]
} deriving (Generic)

newtype PlayerTurnMessage = PlayerTurnMessage {
    _playerTurn :: UUID
} deriving (Generic)

newtype DealtCardsMessage = DealtCardsMessage {
    _playerCards :: [Card]
} deriving (Generic)

newtype PotWinnersMessage = PotWinnersMessage {
    _mapping :: [(Pot, [UUID])]
} deriving (Generic)

newtype PlayersRemovedMessage = PlayersRemovedMessage {
    _removed :: [UUID]
} deriving (Generic)

newtype CardRevealMessage = CardRevealMessage {
    _infos :: [PlayerHandInfo]
} deriving (Generic)

-- TYPES

type GameStateT a = StateT Game IO a

type GameState a = State Game a

-- INSTANCES

instance Binary (ActionMessage a)

instance Binary PlayerTurnMessage

instance Binary CardMessage

instance Binary DealtCardsMessage

instance Binary PotWinnersMessage

instance Binary GameOverMessage

instance Binary PlayersRemovedMessage

instance Binary CardRevealMessage

instance Binary PlayerHandInfo

instance (Binary a, Binary b) => Binary (Hand a b)

instance Binary Pot

instance Binary Card

instance Binary Suit

instance Binary Value

instance Binary (Action a)

instance (Binary a) => Binary (Message a)
