module ClientTypes
(
    StatesNSignals(..),
    CGame(..),
    CGameStateT,
    CGameState
)
where

import Graphics.QML (SignalKey)
import Data.Text (Text)
import Data.IORef (IORef)
import Control.Monad.Trans.State (StateT, State)

import Types (ClientGame)

data CGame = CGame {
    _game :: ClientGame,
    _qmlState :: StatesNSignals
}

data StatesNSignals = StatesNSignals {
    _pCardsSig :: SignalKey (IO ()),
    _pCardsS :: IORef [[Text]],
    _pChipsSig :: SignalKey (IO ()),
    _pChipsS :: IORef [Int],
    _pNamesSig :: SignalKey (IO ()),
    _pNamesS :: IORef [Text],
    _tCardsSig :: SignalKey (IO ()),
    _tCardsS :: IORef [Text],
    _bEnabledSig :: SignalKey (IO ()),
    _bEnabledS :: IORef [Bool],
    _potChipsSig :: SignalKey (IO ()),
    _potChipsS :: IORef Int,
    _pVisibleSig :: SignalKey (IO ()),
    _pVisibleS :: IORef [Bool]
}

type CGameStateT a = StateT CGame IO a
type CGameState a = State CGame a
