{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens

import Types

--don't want to export all the individual functions...
{-# ANN module "Hlint: ignore Use module export list" #-}

makeLenses ''Game
makeLenses ''Player
makeLenses ''Players
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
makeLenses ''Pot
makeLenses ''HandInfo
