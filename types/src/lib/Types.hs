{-# LANGUAGE DeriveGeneric, ExistentialQuantification #-}

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
    PlayersRemovedMsg(..),
    CardRevealMsg(..),
    PlayerHandInfo(..),
    InitialGameMsg(..),
    InputMsg(..),
    MinRaiseMsg(..),
    TextMsg(..),
    ClientGame(..),
    CPlayer(..),
    CBets(..),
    ShuffleType(..),
    Deck(..),
    KnuthDeck(..),
    RandomIndexDeck(..),
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
import Data.Text (Text)
import Data.Char (toLower)
import Data.IORef (IORef)

import Text.Printf 
    (PrintfArg(..), printf, fmtChar, fmtPrecision, vFmt, formatString,
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
    _roundNumber :: Int,
    _shuffleType :: ShuffleType,
    -- can't change shuffle type mid round
    _nextRoundShuffleType :: IORef ShuffleType
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
    _deck :: Deck
}

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
          deriving (Bounded, Enum, Eq, Generic, Show)

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
           deriving (Bounded, Enum, Eq, Ord, Generic, Show)

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
             | MIsNewChips NewChipsMsg
             | MIsGameOver
             | MIsPlayersRemoved PlayersRemovedMsg
             | MIsCardReveal CardRevealMsg
             | MIsInitialGame InitialGameMsg
             | MIsInput InputMsg
             | MIsBadInput
             | MIsGatherChips
             | MIsResetRound
             | MIsMinRaise MinRaiseMsg
             | MIsNextState
             | MIsTextMsg TextMsg
             deriving (Generic, Show)

data ShuffleType = RandomIndex
                 | Knuth 
                 deriving (Show, Eq)

data Deck = IsKnuth KnuthDeck
          | IsRandomIndex RandomIndexDeck

newtype KnuthDeck = KnuthDeck [Card]
newtype RandomIndexDeck = RandomIndexDeck [Card]

-- NEWTYPES

newtype CardMsg = CardMsg [Card] deriving (Generic, Show)
newtype PlayerTurnMsg = PlayerTurnMsg UUID deriving (Generic, Show)
newtype DealtCardsMsg = DealtCardsMsg [Card] deriving (Generic, Show)
newtype NewChipsMsg = NewChipsMsg [(UUID, Int)] deriving (Generic, Show)
newtype PlayersRemovedMsg = PlayersRemovedMsg [UUID] deriving (Generic, Show)
newtype CardRevealMsg = CardRevealMsg [PlayerHandInfo] deriving (Generic, Show)
newtype InitialGameMsg = InitialGameMsg ClientGame deriving (Generic, Show)
newtype InputMsg = InputMsg [Action Int] deriving (Generic, Show)
newtype MinRaiseMsg = MinRaiseMsg Int deriving (Generic, Show)
newtype TextMsg = TextMsg [Text] deriving (Generic, Show)

-- TYPES

type GameStateT a = StateT Game IO a
type GameState m a = StateT Game m a

-- INSTANCES

instance Binary PlayerTurnMsg
instance Binary CardMsg
instance Binary DealtCardsMsg
instance Binary NewChipsMsg
instance Binary PlayersRemovedMsg
instance Binary CardRevealMsg
instance Binary PlayerHandInfo
instance Binary InitialGameMsg
instance Binary InputMsg
instance Binary MinRaiseMsg
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
