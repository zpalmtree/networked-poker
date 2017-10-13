module ClientTypes
(
    PSignals(..),
    CSignals(..),
    CGame(..),
    CCards(..),
    CPlayerQueue(..),
    CPlayer(..),
    CGameStateT,
    CGameState
)
where

import Graphics.QML (SignalKey)
import Data.Text (Text)
import Data.IORef (IORef)
import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (StateT, State)

import Types (Stage, Bets, Card, Hand, Value)

data PSignals = PSignals {
    _nameSig :: SignalKey (IO ()),
    _nameState :: IORef Text,
    _chipSig :: SignalKey (IO ()),
    _chipState :: IORef Int,
    _card1ImgSig :: SignalKey (IO ()),
    _card1ImgState :: IORef Text,
    _card2ImgSig :: SignalKey (IO ()),
    _card2ImgState :: IORef Text
}

data CSignals = CSignals {
    _tCard1ImgSig :: SignalKey (IO ()),
    _tCard1ImgState :: IORef Text,
    _tCard2ImgSig :: SignalKey (IO ()),
    _tCard2ImgState :: IORef Text,
    _tCard3ImgSig :: SignalKey (IO ()),
    _tCard3ImgState :: IORef Text,
    _tCard4ImgSig :: SignalKey (IO ()),
    _tCard4ImgState :: IORef Text,
    _tCard5ImgSig :: SignalKey (IO ()),
    _tCard5ImgState :: IORef Text
}

data CGame = CGame {
    _cplayerQueue :: CPlayerQueue,
    _cstage :: Stage,
    _cardInfo :: CCards,
    _cbets :: Bets
}

data CCards = CCards {
    _communityCards :: [Card],
    _cSigs :: CSignals
}

data CPlayerQueue = CPlayerQueue {
    _cPlayers :: [CPlayer],
    _cDealer :: Int
}

data CPlayer = CPlayer {
    _pName :: String,
    _pUuid :: UUID,
    _pChips :: Int,
    _pCards :: Maybe [Card],
    _pInPlay :: Bool,
    _pAllIn :: Bool,
    _pBet :: Int,
    _pMadeInitialBet :: Bool,
    _pHandValue :: Maybe (Hand Value Value),
    _pCanReRaise :: Bool,
    _IsMe :: Bool,
    _pSigs :: PSignals
}

type CGameStateT a = StateT CGame IO a

type CGameState a = State CGame a
