{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Game = Game {
    _numPlayers :: Int,
    _players :: [Player],
    _playerTurn :: Player,
    _currentState :: State,
    _pot :: Int,
    _tableCards :: [Card],
    _deck :: [Card]
} deriving Show

data Player = Player {
    _name :: String,
    _money :: Int,
    _cards :: (Card, Card),
    _inPlay :: Bool
} deriving Show

data Card = Card {
    _value :: Value,
    _suit :: Suit
} deriving Show

data State = PreFlop | Flop | Turn | River deriving Show

data Suit = Heart | Spade | Club | Diamond deriving (Show, Bounded, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Show, Bounded, Enum)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Card
