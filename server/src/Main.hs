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
    -- in showdown -> no more betting can happen
    | isShowdown game = return $ showdown game
    -- only one player left -> they get the winnings, start next round
    | numInPlay game == 1 = return . nextRound $ giveWinnings index' game
    {- max of one person isn't all in -> no more betting can happen -> deal
    more cards, but can't do anymore betting -}
    | numInPlay game - numAllIn game <= 1 = bettingRound =<< nextState game
    -- player isn't in play, go onto next player 
    | not $ getCurrentPlayer game^.inPlay = bettingRound $ nextPlayer game 
    -- player is in play, and hasn't made their initial bet, so prompt for bet
    | not $ getCurrentPlayer game^.madeInitialBet = bettingRound . nextPlayer
                                                    $ promptBet game
    {- player is in play, and has made initial bet, but isn't matched with
    current bet level -> has to call/fold/raise -}
    | getCurrentPlayer game^.madeInitialBet && 
      getCurrentPlayer game^.bet < game^.bets.currentBet
        = bettingRound . nextPlayer $ promptBet game
    {- else the player has already made their bet so move on to next load of 
    cards and bets -}
    | otherwise = bettingRound =<< nextState game
    where index' = victorIndex game

promptBet :: Game -> Game
promptBet = undefined

showdown :: Game -> Game
showdown = undefined

nextState :: Game -> IO Game
nextState game = case game^.state of
    PreFlop -> revealFlop $ updatePot game
    Flop -> revealTurn $ updatePot game
    Turn -> revealRiver $ updatePot game
    River -> return . updatePot $ game & state .~ Showdown
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
fold game = game & mutateCurrentPlayer game . inPlay .~ False

raise :: Int -> Game -> Game
raise amount game
    | playerChips < amount = error "Not enough chips!"
    | playerChips == amount = goAllIn game
    | otherwise = makeBet amount game
    where playerChips = getCurrentPlayer game^.chips


call :: Game -> Game
call = undefined

nextPlayer :: Game -> Game
nextPlayer game = game & playerInfo.playerTurn .~ advancePlayerTurn game
