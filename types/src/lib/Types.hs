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
    NewChipsMsg(..),
    GameOverMsg(..),
    PlayersRemovedMsg(..),
    CardRevealMsg(..),
    PlayerHandInfo(..),
    InitialGameMsg(..),
    InputMsg(..),
    BadInputMsg(..),
    GatherChipsMsg(..),
    ResetRoundMsg(..),
    MinRaiseMsg(..),
    NextStateMsg(..),
    TextMsg(..),
    ClientGame(..),
    CPlayer(..),
    CBets(..),
    GameStateT,
    GameState
)
where

import Control.Monad.Trans.State (StateT(..))
import Data.UUID.Types (UUID)
import Control.Concurrent.MVar (MVar)
import Network.Socket (Socket)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Data.Text (Text)

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

data CPlayer = CPlayer {
    _cName :: String,
    _cUUID :: UUID,
    _cChips :: Int,
    _cCards :: [Card],
    _cInPlay :: Bool,
    _cAllIn :: Bool,
    _cBet :: Int,
    _cMadeInitialBet :: Bool,
    _cHandValue :: Maybe (Hand Value Value),
    _cCanReRaise :: Bool,
    _cIsMe :: Bool
} deriving (Generic, Show)

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
}

data CBets = CBets {
    _cPot :: Int,
    _cCurrentBet :: Int,
    _cSmallBlindSize :: Int,
    _cBigBlindSize :: Int,
    _cMinimumRaise :: Int
} deriving (Generic, Show)

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
} deriving (Eq, Generic, Show)

data PlayerQueue = PlayerQueue {
    _players :: [Player],
    _dealer :: Int
} deriving (Eq)

data ActionMsg a = ActionMsg {
    _action :: Action a,
    _player :: UUID
} deriving (Generic, Show)

data PlayerHandInfo = PlayerHandInfo {
    _person :: UUID,
    _rank :: Hand Value Value,
    _hand :: [Card]
} deriving (Generic, Show)

data ClientGame = ClientGame {
    _cPlayers :: [CPlayer],
    _cDealer :: UUID,
    _cCurrentPlayer :: UUID,
    _cStage :: Stage,
    _cCommunityCards :: [Card],
    _cBets :: CBets
} deriving (Generic, Show)

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
          deriving (Bounded, Enum, Eq, Generic)

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
           deriving (Bounded, Enum, Eq, Ord, Generic)

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
              deriving (Eq, Ord, Generic, Show)

data Message = MIsAction (ActionMsg Int)
             | MIsPlayerTurn PlayerTurnMsg
             | MIsCard CardMsg
             | MIsDealt DealtCardsMsg
             | MIsNewChips NewChipsMsg
             | MIsGameOver GameOverMsg
             | MIsPlayersRemoved PlayersRemovedMsg
             | MIsCardReveal CardRevealMsg
             | MIsInitialGame InitialGameMsg
             | MIsInput InputMsg
             | MIsBadInput BadInputMsg
             | MIsGatherChips GatherChipsMsg
             | MIsResetRound ResetRoundMsg
             | MIsMinRaise MinRaiseMsg
             | MIsNextState NextStateMsg
             | MIsTextMsg TextMsg
             deriving (Generic, Show)

data GameOverMsg = GameOverMsg deriving (Generic, Show)
data BadInputMsg = BadInputMsg deriving (Generic, Show)
data GatherChipsMsg = GatherChipsMsg deriving (Generic, Show)
data ResetRoundMsg = ResetRoundMsg deriving (Generic, Show)
data NextStateMsg = NextStateMsg deriving (Generic, Show)

-- NEWTYPES

newtype CardMsg = CardMsg {
    _allCards :: [Card]
} deriving (Generic, Show)

newtype PlayerTurnMsg = PlayerTurnMsg {
    _playerTurn :: UUID
} deriving (Generic, Show)

newtype DealtCardsMsg = DealtCardsMsg {
    _playerCards :: [Card]
} deriving (Generic, Show)

newtype NewChipsMsg = NewChipsMsg {
    _mapping :: [(UUID, Int)]
} deriving (Generic, Show)

newtype PlayersRemovedMsg = PlayersRemovedMsg {
    _removed :: [UUID]
} deriving (Generic, Show)

newtype CardRevealMsg = CardRevealMsg {
    _infos :: [PlayerHandInfo]
} deriving (Generic, Show)

newtype InitialGameMsg = InitialGameMsg {
    _clientGame :: ClientGame
} deriving (Generic, Show)

-- a list of valid actions client can perform
newtype InputMsg = InputMsg {
    _imsg :: [Action Int]
} deriving (Generic, Show)

newtype MinRaiseMsg = MinRaiseMsg {
    _minRaise :: Int
} deriving (Generic, Show)

newtype TextMsg = TextMsg {
    _textMsg :: Text
} deriving (Generic, Show)

-- TYPES

type GameStateT a = StateT Game IO a

type GameState m a = StateT Game m a

-- INSTANCES

instance Binary PlayerTurnMsg
instance Binary CardMsg
instance Binary DealtCardsMsg
instance Binary NewChipsMsg
instance Binary GameOverMsg
instance Binary PlayersRemovedMsg
instance Binary CardRevealMsg
instance Binary PlayerHandInfo
instance Binary InitialGameMsg
instance Binary InputMsg
instance Binary BadInputMsg
instance Binary GatherChipsMsg
instance Binary ResetRoundMsg
instance Binary MinRaiseMsg
instance Binary NextStateMsg
instance Binary TextMsg
instance Binary Pot
instance Binary Card
instance Binary Value
instance Binary Suit
instance Binary (Action a)
instance Binary ClientGame
instance Binary CPlayer
instance Binary Stage
instance Binary CBets
instance Binary Message
instance Binary (ActionMsg a)
instance (Binary a, Binary b) => Binary (Hand a b)

instance Eq (Action a) where
    (==) Fold Fold = True
    (==) Check Check = True
    (==) Call Call = True
    (==) (Raise _) (Raise _) = True
    (==) AllIn AllIn = True
    (==) SmallBlind SmallBlind = True
    (==) BigBlind BigBlind = True
    (==) _ _ = False

instance Show Card where
    show (Card v s) = printf "assets/card-%s-%ss.png" (show v) (show s)

instance Show Value where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "jack"
    show Queen = "queen"
    show King = "king"
    show Ace = "ace"

instance Show Suit where
    show Club = "club"
    show Diamond = "diamond"
    show Heart = "heart"
    show Spade = "spade"
