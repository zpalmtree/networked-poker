{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens (makeLenses)

import Types (Game, Player, Players, Cards, Bets, Card, HandInfo, Pot)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Players
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
makeLenses ''HandInfo
makeLenses ''Pot
