module Main where

import Types
import Control.Lens
import Betting
import PlayerUtilities
import CardUtilities
import StateUtilities

main :: IO ()
main = return ()

bettingRound :: Game -> IO Game
bettingRound game
    | isShowdown game = return $ showdown game
    | numInPlay game == 1 = return . nextRound $ giveWinnings index' game
    | numInPlay game - numAllIn game <= 1 = bettingRound =<< nextState game
    where index' = victorIndex game

showdown :: Game -> Game
showdown = undefined

nextState :: Game -> IO Game
nextState game = case game^.state of
    PreFlop -> revealFlop game
    Flop -> revealTurn game
    Turn -> revealRiver game
    River -> return $ game & state .~ Showdown
    _ -> error "Programming error in nextState"

giveWinnings :: Int -> Game -> Game
giveWinnings player game = game & playerInfo.players.ix player.chips
                                  +~ gatherChips game

nextRound :: Game -> Game
nextRound game = game & allPlayers.cards .~ Nothing
                      & allPlayers.inPlay .~ True
                      & allPlayers.bet .~ 0
                      & playerInfo.dealer .~ advanceDealer game
                      & playerInfo.playerTurn .~ advancePlayerTurn game
                      & state .~ PreFlop
                      & cardInfo .~ Cards Nothing fullDeck
                      & roundDone .~ False
                      & bets.pot .~ 0
                      & bets.currentBet .~ 0
    where allPlayers = playerInfo.players.traversed

{-
bettingRound :: Game -> IO Game
bettingRound game
    | not $ currentPlayer^.inPlay = go return
    | currentPlayer^.bet >= game^.bets.currentBet = go foldCheckRaise
    | otherwise = go foldCallRaise 
    where currentPlayer = game^.playerInfo.players ^?! ix player
          player = game^.playerInfo.playerTurn
          go f = do
            newState <- f game
            return $ nextPlayer newState
-}

foldCheckRaise :: Game -> IO Game
foldCheckRaise game = do
    putStrLn "fold, check, or raise?"
    input <- getLine
    case input of
        "fold" -> return $ fold game
        "check" -> return game
        {- this will actually be input from the network, so we'll do some fancy
        parsing, for now just passing a fixed value to test -}
        "raise" -> return $ raise 100 game
        _ -> putStrLn "bad input" >> foldCheckRaise game


foldCallRaise :: Game -> IO Game
foldCallRaise game = do
    putStrLn "fold, call, or raise?"
    input <- getLine
    case input of
        "fold" -> return $ fold game
        "call" -> return $ call game
        "raise" -> return $ raise 100 game
        _ -> putStrLn "bad input" >> foldCheckRaise game

fold :: Game -> Game
fold game = game & playerInfo.players.ix player.inPlay .~ False
    where player = game^.playerInfo.playerTurn

raise :: Int -> Game -> Game
raise amount game
    | playerChips < amount = error "Not enough chips!"
    | playerChips == amount = goAllIn player game
    | otherwise = makeBet amount player game
    where player = game^.playerInfo.playerTurn
          playerChips = game^.playerInfo.players ^?! ix player.chips

call :: Game -> Game
call = undefined

nextPlayer :: Game -> Game
nextPlayer game = game & playerInfo.playerTurn .~ advancePlayerTurn game
