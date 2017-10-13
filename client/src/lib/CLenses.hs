{-# LANGUAGE TemplateHaskell #-}

module CLenses where

import Control.Lens (makeLenses)

import ClientTypes (CGame, StatesNSignals)

makeLenses ''CGame
makeLenses ''StatesNSignals
