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
import Control.Lens hiding (Fold)

import Types
import Output.Terminal.OutputMessages
import Output.Terminal.OutputUtilities (playerNum, turnCard, riverCard, 
                                        playerCards, potWinners, printHand)

import PlayerUtilities (getCurrentPlayer)

import Lenses (name, bets, currentBet, chips, bet, name, cardInfo, tableCards,
               playerInfo, depreciatedPlayers, pots, pot, inPlay, roundNumber,
               smallBlindSize, bigBlindSize)

outputAction :: Game -> Action Int -> IO ()
outputAction game action = case action of
    Fold -> putStrLn $ printf actionFold num playerName
    Check -> putStrLn $ printf actionCheck num playerName
    Call -> putStrLn $ printf actionCall num playerName bet'
    Raise a -> putStrLn $ printf actionRaise num playerName bet' a
    AllIn -> putStrLn $ printf actionAllIn num playerName chips'
    where num = playerNum $ getCurrentPlayer game
          playerName = getCurrentPlayer game^.name
          bet' = game^.bets.currentBet
          chips' = getCurrentPlayer game^.chips + getCurrentPlayer game^.bet

outputPlayerTurn :: Game -> IO ()
outputPlayerTurn game = putStrLn $ printf playersTurn num playerName
    where playerName = getCurrentPlayer game^.name
          num = playerNum $ getCurrentPlayer game

{-# ANN outputFlop "HLint: ignore Use head" #-}
outputFlop :: Game -> IO ()
outputFlop game = putStrLn $ printf flopCards card1 card2 card3
    where cards = game^.cardInfo.tableCards
          card1 = show $ cards !! 0
          card2 = show $ cards !! 1
          card3 = show $ cards !! 2

outputTurn :: Game -> IO ()
outputTurn game = putStrLn . turnCard $ map show cards
    where cards = game^.cardInfo.tableCards

outputRiver :: Game -> IO ()
outputRiver game = putStrLn . riverCard $ map show cards
    where cards = game^.cardInfo.tableCards

outputPlayerCards :: Game -> IO ()
outputPlayerCards game = putStrLn $ playerCards players'
    where players' = game^..playerInfo.depreciatedPlayers.traversed

--have to reimplement gatherchips because betting imports this module
outputWinner :: Game -> Player -> IO ()
outputWinner game p = putStrLn $ printf winner (playerNum p) (p^.name) chips'
    where chips' = sum (game^..bets.pots.traversed.pot)
                 + sum (game^..playerInfo.depreciatedPlayers.traversed.bet)

outputWinners :: Game -> [(Pot, [Player])] -> IO ()
outputWinners _ = mapM_ (putStrLn . potWinners)

outputGameOver :: Game -> IO ()
outputGameOver game = putStrLn msg
    where winner' = maximumBy (compare `on` (^.chips)) players'
          players' = game^.playerInfo.depreciatedPlayers
          msg = printf totalWinner num name' chips'
          name' = winner'^.name
          num = playerNum winner'
          chips' = winner'^.chips

outputPlayersRemoved :: Game -> Maybe [Player] -> IO ()
outputPlayersRemoved _ = mapM_ (mapM_ $ putStrLn . helper)
    where helper p = printf playerRemoved (playerNum p) (p^.name)

outputHandValues :: Game -> IO ()
outputHandValues game = mapM_ (putStrLn . printHand) inPlayers
    where inPlayers = filter (^.inPlay) (game^.playerInfo.depreciatedPlayers)

outputRoundNumber :: Game -> IO ()
outputRoundNumber game = putStrLn $ printf roundNumberMsg num
    where num = game^.roundNumber

outputSmallBlindMade :: Game -> IO ()
outputSmallBlindMade game = outputBlindMade game smallBlind size
    where size = game^.bets.smallBlindSize

outputBigBlindMade :: Game -> IO ()
outputBigBlindMade game = outputBlindMade game bigBlind size
    where size = game^.bets.bigBlindSize

outputBlindMade :: Game -> String -> Int -> IO ()
outputBlindMade game f size = putStrLn $ printf f num name' size
    where num = playerNum $ getCurrentPlayer game
          name' = getCurrentPlayer game^.name
