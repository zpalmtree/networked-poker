{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens (makeLenses)

import Types 
    (Game, Player, Cards, Bets, Card, HandInfo, Pot, PlayerQueue, ClientGame,
     ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, NewChipsMsg,
     PlayersRemovedMsg, CardRevealMsg, PlayerHandInfo, InitialGameMsg,
     CPlayer, InputMsg, CBets, MinRaiseMsg, TextMsg)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
makeLenses ''HandInfo
makeLenses ''Pot
makeLenses ''PlayerQueue
makeLenses ''ClientGame
makeLenses ''ActionMsg
makeLenses ''PlayerTurnMsg
makeLenses ''CardMsg
makeLenses ''DealtCardsMsg
makeLenses ''NewChipsMsg
makeLenses ''PlayersRemovedMsg
makeLenses ''CardRevealMsg
makeLenses ''PlayerHandInfo
makeLenses ''InitialGameMsg
makeLenses ''MinRaiseMsg
makeLenses ''TextMsg
makeLenses ''CPlayer
makeLenses ''InputMsg
makeLenses ''CBets
