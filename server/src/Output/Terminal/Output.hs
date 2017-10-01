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
import Safe (at)
import Data.UUID.Types (UUID)

import Types (GameStateT, Action(..), Player, Pot)
import Utilities.Player (getCurrentPlayerT, getPlayerByUUID)

import Output.Terminal.OutputMessages
    (actionFold, actionCheck, actionCall, actionRaise, actionAllIn,
     playersTurn, winner, flopCards, totalWinner, playerRemoved, 
     roundNumberMsg, smallBlind, bigBlind)

import Utilities.Terminal.Output
    (turnCard, riverCard, playerCards, potWinners, printHand)

import Lenses
    (name, bets, currentBet, chips, bet, name, cardInfo, tableCards, pots, 
     pot, inPlay, roundNumber, smallBlindSize, bigBlindSize, playerQueue, 
     players)

outputAction :: Action Int -> GameStateT ()
outputAction action = do
    s <- get

    player <- getCurrentPlayerT

    let playerName = player^.name
        bet' = s^.bets.currentBet
        chips' = player^.chips + player^.bet

    case action of
        Fold -> lift . putStrLn $ printf actionFold playerName
        Check -> lift . putStrLn $ printf actionCheck playerName
        Call -> lift . putStrLn $ printf actionCall playerName bet'
        Raise n -> lift . putStrLn $ printf actionRaise playerName bet' n
        AllIn -> lift . putStrLn $ printf actionAllIn playerName chips'

outputPlayerTurn :: GameStateT ()
outputPlayerTurn = do
    player <- getCurrentPlayerT

    lift . putStrLn $ printf playersTurn (player^.name)

outputFlop :: GameStateT ()
outputFlop = do
    s <- get

    let cards = s^.cardInfo.tableCards
        card1 = show $ cards `at` 0
        card2 = show $ cards `at` 1
        card3 = show $ cards `at` 2

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

outputWinner :: UUID -> GameStateT ()
outputWinner uuid' = do
    s <- get

    p <- getPlayerByUUID uuid'
    
    let chips' = sum (s^..bets.pots.traversed.pot)
               + sum (s^..playerQueue.players.traversed.bet)

    lift . putStrLn $ printf winner (p^.name) chips'

outputWinners :: [(Pot, [Player])] -> GameStateT ()
outputWinners = mapM_ (lift . putStrLn . potWinners)

outputGameOver :: GameStateT ()
outputGameOver = do
    s <- get
    
    let players' = s^.playerQueue.players
        winner' = maximumBy (compare `on` (^.chips)) players'
        name' = winner'^.name
        chips' = winner'^.chips

    lift . putStrLn $ printf totalWinner name' chips'

outputPlayersRemoved :: Maybe [Player] -> GameStateT ()
outputPlayersRemoved = mapM_ (mapM_ (lift . putStrLn . helper))
    where helper p = printf playerRemoved (p^.name)

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

    lift . putStrLn $ printf f (player^.name) size
