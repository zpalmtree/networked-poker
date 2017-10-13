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
    _buttonsSig :: SignalKey (IO ()),
    _buttonsS :: IORef [Bool],
    _potSig :: SignalKey (IO ()),
    _potS :: IORef Int,
    _pEnabledSig :: SignalKey (IO ()),
    _pEnabled :: IORef [Bool]
}

type CGameStateT a = StateT CGame IO a
type CGameState a = State CGame a
