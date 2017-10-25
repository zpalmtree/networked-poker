module ClientTypes
(
    StatesNSignals(..),
    CGame(..),
    CGameStateT,
    CGameState
)
where

import Control.Concurrent.MVar (MVar)
import Graphics.QML (SignalKey, ObjRef)
import Data.Text (Text)
import Data.IORef (IORef)
import Control.Monad.Trans.State (StateT)

import Types (ClientGame, Action)

data CGame = CGame {
    _game :: ClientGame,
    _qmlState :: StatesNSignals,
    _ctx :: ObjRef ()
}

data StatesNSignals = StatesNSignals {
    _pCardsSig :: SignalKey (IO ()),
    _pCardsS :: IORef [[Text]],
    _pBetsSig :: SignalKey (IO ()),
    _pBetsS :: IORef [Int],
    _pNamesSig :: SignalKey (IO ()),
    _pNamesS :: IORef [Text],
    _tCardsSig :: SignalKey (IO ()),
    _tCardsS :: IORef [Text],
    _bEnabledSig :: SignalKey (IO ()),
    _bEnabledS :: IORef [Bool],
    _potChipsSig :: SignalKey (IO ()),
    _potChipsS :: IORef Int,
    _pVisibleSig :: SignalKey (IO ()),
    _pVisibleS :: IORef [Bool],
    _pInPlaySig :: SignalKey (IO ()),
    _pInPlayS :: IORef [Bool],
    _pCurrentPlayerSig :: SignalKey (IO ()),
    _pCurrentPlayerS :: IORef [Bool],
    _slideMinSig :: SignalKey (IO ()),
    _slideMinS :: IORef Int,
    _slideMaxSig :: SignalKey (IO ()),
    _slideMaxS :: IORef Int,
    _lossWindowVisibleSig :: SignalKey (IO ()),
    _lossWindowVisibleS :: IORef Bool,
    _winWindowVisibleSig :: SignalKey (IO ()),
    _winWindowVisibleS :: IORef Bool,
    _logMsgSig :: SignalKey (IO ()),
    _logMsgS :: IORef [Text],
    _actionMade :: MVar (Action Int)
}

type CGameStateT a = StateT CGame IO a
type CGameState m a = StateT CGame m a
