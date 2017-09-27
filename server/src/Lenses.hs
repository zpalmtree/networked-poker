{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens (makeLenses)

import Types (Game, Player, Cards, Bets, Card, HandInfo, Pot, PlayerQueue)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
makeLenses ''HandInfo
makeLenses ''Pot
makeLenses ''PlayerQueue
