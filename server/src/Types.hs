{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Game = Game {
    _playerInfo :: Players,
    _state :: State,
    _cardInfo :: Cards,
    _roundDone :: Bool,
    _bets :: Bets
} deriving Show

data Player = Player {
    _num :: Int,
    _name :: String,
    _chips :: Int,
    _cards :: Maybe (Card, Card),
    _inPlay :: Bool,
    _allIn :: Bool,
    _bet :: Int,
    _madeInitialBet :: Bool
} deriving Show

data Players = Players {
    _numPlayers :: Int,
    _players :: [Player],
    _playerTurn :: Int,
    _dealer :: Int
} deriving Show

data Cards = Cards {
    _tableCards :: Maybe [Card],
    _deck :: [Card]
} deriving Show

data Bets = Bets {
    _pot :: Int,
    _currentBet :: Int,
    _smallBlindSize :: Int,
    _bigBlindSize :: Int
} deriving Show

data Card = Card {
    _value :: Value,
    _suit :: Suit
} deriving Show

data State = PreFlop | Flop | Turn | River | Showdown deriving Show

data Suit = Heart | Spade | Club | Diamond deriving (Show, Bounded, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Show, Bounded, Enum)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Players
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
