{-# LANGUAGE TemplateHaskell #-}

module CLenses where

import Control.Lens (makeLenses)

import ClientTypes (PSignals, CSignals, CGame, CCards, CPlayerQueue, CPlayer)

makeLenses ''PSignals
makeLenses ''CSignals
makeLenses ''CGame
makeLenses ''CCards
makeLenses ''CPlayerQueue
makeLenses ''CPlayer
