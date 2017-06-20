{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

--don't want to export all the individual functions...
{-# ANN module "Hlint: ignore Use module export list" #-}

data Game = Game {
    _playerInfo :: Players,
    _state :: State,
    _cardInfo :: Cards,
    _roundDone :: Bool,
    _bets :: Bets,
    _gameFinished :: Bool,
    _roundNumber :: Int
} deriving Show

data Player = Player {
    _name :: String,
    _num :: Int,
    _chips :: Int,
    _cards :: [Card],
    _inPlay :: Bool,
    _allIn :: Bool,
    _bet :: Int,
    _madeInitialBet :: Bool,
    _hand :: [Card],
    _handValue :: Maybe Hand,
    _canReRaise :: Bool
} deriving Show

data Players = Players {
    _numPlayers :: Int,
    _players :: [Player],
    _dealer :: Int,
    _playerTurn :: Int
} deriving Show

data Cards = Cards {
    _tableCards :: [Card],
    _deck :: [Card]
} deriving Show

data Bets = Bets {
    _pots :: [Pot],
    _currentBet :: Int,
    _smallBlindSize :: Int,
    _bigBlindSize :: Int,
    _minimumRaise :: Int
} deriving Show

data Card = Card {
    _value :: Value,
    _suit :: Suit
} deriving Eq

instance Show Card where
    show (Card value' suit') = show value' ++ " of " ++ show suit' ++ "s"

data Pot = Pot {
    _pot :: Int,
    _playerIDs :: [Int]
} deriving Show

data Action a = Fold | Check | Call | Raise a | AllIn deriving Show

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
makeLenses ''Pot
