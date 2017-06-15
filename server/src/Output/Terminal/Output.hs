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
    outputPlayersRemoved
)
where

import Types
import Output.Terminal.OutputMessages
import PlayerUtilities

import Text.Printf
import Control.Lens hiding (Fold)

outputAction :: Game -> Action Int -> IO ()
outputAction game action = case action of
    Fold -> printf actionFold playerNum playerName
    Check -> printf actionCheck playerNum playerName
    Call -> printf actionCall playerNum playerName bet'
    Raise a -> printf actionRaise playerNum playerName bet' (bet' + a)
    AllIn -> printf actionAllIn playerNum playerName chips'
    where playerNum = getCurrentPlayer game^.num + 1 -- 0 indexed
          playerName = getCurrentPlayer game^.name
          bet' = game^.bets.currentBet
          chips' = getCurrentPlayer game^.chips + getCurrentPlayer game^.bet

outputPlayerTurn :: Game -> IO ()
outputPlayerTurn game = printf playersTurn playerNum playerName
    where playerNum = getCurrentPlayer game^.num + 1
          playerName = getCurrentPlayer game^.name

{-# ANN outputFlop "HLint: ignore Use head" #-}
outputFlop :: Game -> IO ()
outputFlop game = printf flopCards card1 card2 card3
    where cards' = game^.cardInfo.tableCards
          card1 = show $ cards' !! 0
          card2 = show $ cards' !! 1
          card3 = show $ cards' !! 2

outputTurn :: Game -> IO ()
outputTurn game = putStrLn . turnCard $ map show cards'
    where cards' = game^.cardInfo.tableCards

outputRiver :: Game -> IO ()
outputRiver game = putStrLn . riverCard $ map show cards'
    where cards' = game^.cardInfo.tableCards

outputPlayerCards :: Game -> IO ()
outputPlayerCards = undefined

outputWinner :: Game -> Player -> IO ()
outputWinner = undefined

outputWinners :: Game -> [(Pot, [Player])] -> IO ()
outputWinners = undefined

outputGameOver :: Game -> IO ()
outputGameOver = undefined

outputPlayersRemoved :: Game -> Maybe [Player] -> IO ()
outputPlayersRemoved = undefined
