module Output.Terminal.Output
(
    outputAction,
    outputPlayerTurn,
    outputFlop,
    outputTurn,
    outputRiver,
    outputPlayerCards,
    outputWinner,
    outputWinners,
    outputGameOver,
    outputPlayersRemoved,
    outputHandValues,
    outputRoundNumber,
    outputSmallBlindMade,
    outputBigBlindMade
)
where

import Text.Printf (printf)
import Data.Function (on)
import Data.List (maximumBy)
import Control.Lens ((^.), (^..), traversed)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)

import Types (GameStateT, Action(..), Player, Pot)
import Utilities.Player (getCurrentPlayerT)

import Output.Terminal.OutputMessages
    (actionFold, actionCheck, actionCall, actionRaise, actionAllIn,
     playersTurn, winner, flopCards, totalWinner, playerRemoved, 
     roundNumberMsg, smallBlind, bigBlind)

import Utilities.Terminal.Output
    (playerNum, turnCard, riverCard, playerCards, potWinners, printHand)

import Lenses
    (name, bets, currentBet, chips, bet, name, cardInfo, tableCards, pots, 
     pot, inPlay, roundNumber, smallBlindSize, bigBlindSize, playerQueue, 
     players)

outputAction :: Action Int -> GameStateT ()
outputAction action = do
    s <- get

    player <- getCurrentPlayerT

    let num = playerNum player
        playerName = player^.name
        bet' = s^.bets.currentBet
        chips' = player^.chips + player^.bet

    case action of
        Fold -> lift . putStrLn $ printf actionFold num playerName
        Check -> lift . putStrLn $ printf actionCheck num playerName
        Call -> lift . putStrLn $ printf actionCall num playerName bet'
        Raise n -> lift . putStrLn $ printf actionRaise num playerName bet' n
        AllIn -> lift . putStrLn $ printf actionAllIn num playerName chips'

outputPlayerTurn :: GameStateT ()
outputPlayerTurn = do
    player <- getCurrentPlayerT

    let playerName = player^.name
        num = playerNum player

    lift . putStrLn $ printf playersTurn num playerName

{-# ANN outputFlop "HLint: ignore Use head" #-}
outputFlop :: GameStateT ()
outputFlop = do
    s <- get

    let cards = s^.cardInfo.tableCards
        card1 = show $ cards !! 0
        card2 = show $ cards !! 1
        card3 = show $ cards !! 2

    lift . putStrLn $ printf flopCards card1 card2 card3

outputTurn :: GameStateT ()
outputTurn = outputCardMsg turnCard

outputRiver :: GameStateT ()
outputRiver = outputCardMsg riverCard

outputCardMsg :: ([String] -> String) -> GameStateT ()
outputCardMsg msg = do
    s <- get

    lift . putStrLn . msg $ map show (s^.cardInfo.tableCards)

outputPlayerCards :: GameStateT ()
outputPlayerCards = do
    s <- get

    lift . putStrLn $ playerCards (s^.playerQueue.players)

outputWinner :: Player -> GameStateT ()
outputWinner p = do
    s <- get
    
    let chips' = sum (s^..bets.pots.traversed.pot)
               + sum (s^..playerQueue.players.traversed.bet)

    lift . putStrLn $ printf winner (playerNum p) (p^.name) chips'

outputWinners :: [(Pot, [Player])] -> GameStateT ()
outputWinners = mapM_ (lift . putStrLn . potWinners)

outputGameOver :: GameStateT ()
outputGameOver = do
    s <- get
    
    let players' = s^.playerQueue.players
        winner' = maximumBy (compare `on` (^.chips)) players'
        name' = winner'^.name
        num = playerNum winner'
        chips' = winner'^.chips

    lift . putStrLn $ printf totalWinner num name' chips'

outputPlayersRemoved :: Maybe [Player] -> GameStateT ()
outputPlayersRemoved = mapM_ (mapM_ (lift . putStrLn . helper))
    where helper p = printf playerRemoved (playerNum p) (p^.name)

outputHandValues :: GameStateT ()
outputHandValues = do
    s <- get
    
    let inPlayers = filter (^.inPlay) (s^.playerQueue.players)

    mapM_ (lift . putStrLn . printHand) inPlayers

outputRoundNumber :: GameStateT ()
outputRoundNumber = do
    s <- get
    lift . putStrLn $ printf roundNumberMsg (s^.roundNumber)

outputSmallBlindMade :: GameStateT ()
outputSmallBlindMade = do
    s <- get
    outputBlindMade smallBlind (s^.bets.smallBlindSize)

outputBigBlindMade :: GameStateT ()
outputBigBlindMade = do
    s <- get
    outputBlindMade bigBlind (s^.bets.bigBlindSize)

outputBlindMade :: String -> Int -> GameStateT ()
outputBlindMade f size = do
    player <- getCurrentPlayerT

    lift . putStrLn $ printf f (playerNum player) (player^.name) size
