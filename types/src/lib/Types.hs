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
    ActionMsg(..),
    PlayerTurnMsg(..),
    CardMsg(..),
    DealtCardsMsg(..),
    PotWinnersMsg(..),
    GameOverMsg(..),
    PlayersRemovedMsg(..),
    CardRevealMsg(..),
    PlayerHandInfo(..),
    InitialGameMsg(..),
    InputMsg(..),
    BadInputMsg(..),
    ClientGame(..),
    ClientPlayerQueue(..),
    ClientPlayer,
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

data ClientPlayer = ClientPlayer {
    _clientName :: String,
    _clientUUID :: UUID,
    _clientChips :: Int,
    _clientCards :: Maybe [Card],
    _clientInPlay :: Bool,
    _clientAllIn :: Bool,
    _clientBet :: Int,
    _clientMadeInitialBet :: Bool,
    _clientHandValue :: Maybe (Hand Value Value),
    _clientCanReRaise :: Bool,
    _clientIsMe :: Bool
} deriving (Generic)

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
} deriving (Eq, Generic)

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

data ClientPlayerQueue = ClientPlayerQueue {
    _clientPlayers :: [ClientPlayer],
    _clientDealer :: Int
} deriving (Generic)

data ActionMsg a = ActionMsg {
    _action :: Action a,
    _player :: UUID
} deriving (Generic)

data PlayerHandInfo = PlayerHandInfo {
    _id :: UUID,
    _rank :: Hand Value Value,
    _hand :: [Card]
} deriving (Generic)

data ClientGame = CGame {
    _clientPlayerQueue :: ClientPlayerQueue,
    _clientStage :: Stage,
    _clientCommunityCards :: [Card],
    _clientBets :: Bets
} deriving (Generic)

data Stage = PreFlop 
           | Flop 
           | Turn 
           | River 
           | Showdown 
           deriving (Show, Eq, Generic)

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

data Message = MIsAction (ActionMsg Int)
             | MIsPlayerTurn PlayerTurnMsg
             | MIsCard CardMsg
             | MIsDealt DealtCardsMsg
             | MIsPotWinners PotWinnersMsg
             | MIsGameOver GameOverMsg
             | MIsPlayersRemoved PlayersRemovedMsg
             | MIsCardReveal CardRevealMsg
             | MIsInitialGame InitialGameMsg
             | MIsInput InputMsg
             | MIsBadInput BadInputMsg
             deriving (Generic)

data GameOverMsg = GameOverMsg deriving (Generic)

data BadInputMsg = BadInputMsg deriving (Generic)

-- NEWTYPES

newtype CardMsg = CardMsg {
    _allCards :: [Card]
} deriving (Generic)

newtype PlayerTurnMsg = PlayerTurnMsg {
    _playerTurn :: UUID
} deriving (Generic)

newtype DealtCardsMsg = DealtCardsMsg {
    _playerCards :: [Card]
} deriving (Generic)

newtype PotWinnersMsg = PotWinnersMsg {
    _mapping :: [(Pot, [UUID])]
} deriving (Generic)

newtype PlayersRemovedMsg = PlayersRemovedMsg {
    _removed :: [UUID]
} deriving (Generic)

newtype CardRevealMsg = CardRevealMsg {
    _infos :: [PlayerHandInfo]
} deriving (Generic)

newtype InitialGameMsg = InitialGameMsg {
    _clientGame :: ClientGame
} deriving (Generic)

newtype InputMsg = InputMsg {
    _imsg :: [Action Int]
} deriving (Generic)

-- TYPES

type GameStateT a = StateT Game IO a

type GameState a = State Game a

-- INSTANCES

instance Binary (ActionMsg a)

instance Binary PlayerTurnMsg

instance Binary CardMsg

instance Binary DealtCardsMsg

instance Binary PotWinnersMsg

instance Binary GameOverMsg

instance Binary PlayersRemovedMsg

instance Binary CardRevealMsg

instance Binary PlayerHandInfo

instance Binary InitialGameMsg

instance Binary InputMsg

instance Binary BadInputMsg

instance (Binary a, Binary b) => Binary (Hand a b)

instance Binary Pot

instance Binary Card

instance Binary Suit

instance Binary Value

instance Binary (Action a)

instance Binary ClientGame

instance Binary ClientPlayer

instance Binary ClientPlayerQueue

instance Binary Stage

instance Binary Bets

instance Binary Message

instance Eq (Action a) where
    (==) Fold Fold = True
    (==) Check Check = True
    (==) Call Call = True
    (==) (Raise _) (Raise _) = True
    (==) AllIn AllIn = True
    (==) SmallBlind SmallBlind = True
    (==) BigBlind BigBlind = True
    (==) _ _ = False
