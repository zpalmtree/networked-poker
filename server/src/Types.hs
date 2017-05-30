{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Game = Game {
    _playerInfo :: Players,
    _state :: State,
    _cardInfo :: Cards,
    _roundDone :: Bool,
    _bets :: Bets,
    _gameFinished :: Bool
} deriving Show

data Player = Player {
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
} deriving Eq

instance Show Card where
    show (Card value' suit') = show value' ++ " of " ++ show suit' ++ "s"

data Action = Fold | Check | Call | Raise deriving Show

data Hand = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush |
            FullHouse | FourOfAKind | StraightFlush deriving (Show, Eq, Ord)

data State = PreFlop | Flop | Turn | River | Showdown deriving Show

data Suit = Heart | Spade | Club | Diamond deriving (Show, Bounded, Enum, Eq)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Show, Bounded, Enum, Eq, Ord)

makeLenses ''Game
makeLenses ''Player
makeLenses ''Players
makeLenses ''Cards
makeLenses ''Bets
makeLenses ''Card
