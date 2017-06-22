module Main
(
    main
)
where

import Types
import Betting
import PlayerUtilities
import CardUtilities
import StateUtilities
import Showdown
import TestStates
import Output.Terminal.Output

import Control.Monad
import Control.Lens

main :: IO ()
main = play

play :: IO ()
play = do
    initial <- setup
    final <- gameLoop initial
    void $ cleanup final

gameLoop :: Game -> IO Game
gameLoop game = do
    newGame <- nextRound =<< playRound' game
    if newGame^.gameFinished
        then return game
        else do
            outputRoundNumber newGame
            gameLoop =<< dealCards newGame

setup :: IO Game
setup = do
    let game = setup'
    outputRoundNumber game
    dealCards game

setup' :: Game
setup' = initialGame smallBlindSize' $ initialPlayers players'
    where players' = [testPlayer1, testPlayer2, testPlayer3, testPlayer4]
          smallBlindSize' = 10

cleanup :: Game -> IO ()
cleanup = outputGameOver

playRound' :: Game -> IO Game
playRound' game = do
    newState <- nextPlayer <$> smallBlind game
    playRound =<< nextPlayer <$> bigBlind newState

playRound :: Game -> IO Game
playRound game
    -- in showdown -> no more betting can happen
    | isShowdown game = do
        let (newState, winnerMapping) = showdown game
        outputHandValues newState
        outputWinners newState winnerMapping
        return newState
    -- only one player left -> they get the winnings, start next round
    | numInPlay game == 1 = do
        outputWinner game winner
        return $ giveWinnings (winner, losers) game
    -- max of one person isn't all in -> no more betting can happen -> deal
    -- more cards, but can't do anymore betting
    | numInPlay game - numAllIn game <= 1 = playRound =<< nextState game

    -- player isn't in play, go onto next player 
    | not $ getCurrentPlayer game^.inPlay = playRound $ nextPlayer game 

    -- player is in play, and hasn't made their initial bet, so prompt for bet
    -- they can check because their bet is equal to the current bet - i.e.
    -- they were big blind
    | not (getCurrentPlayer game^.madeInitialBet) 
       && (getCurrentPlayer game^.bet == game^.bets.currentBet) 
      = playRound . nextPlayer =<< promptBet game True

    -- player is in play, and hasn't made their intial bet, so prompt for bet
    -- isn't matched with current bet, so can't check
    | not $ getCurrentPlayer game^.madeInitialBet 
      = playRound . nextPlayer =<< promptBet game False

    -- player is in play, and has made initial bet, but isn't matched with
    -- current bet level -> has to call/fold/raise
    | getCurrentPlayer game^.madeInitialBet && 
      getCurrentPlayer game^.bet < game^.bets.currentBet
      = playRound . nextPlayer =<< promptBet game False

    -- else the player has already made their bet so move on to next load of 
    -- cards and bets
    | otherwise = playRound =<< nextState game

    where (winner, losers) = victor (game^.playerInfo.players)

showdown :: Game -> (Game, [(Pot, [Player])])
showdown game = getWinnersAndDistribute results (results^.bets.pots) []
    where results = getHandValue game

getWinnersAndDistribute :: Game -> [Pot] -> [(Pot, [Player])]
                                -> (Game, [(Pot, [Player])])
getWinnersAndDistribute game [] acc = (game, acc)
getWinnersAndDistribute game (pot':pots') acc = go
    where (newGame, winnerMapping) = distributePot game pot'
          newAcc = winnerMapping : acc
          go = getWinnersAndDistribute newGame pots' newAcc

nextState :: Game -> IO Game
nextState game = case nextState' game^.state of
    PreFlop -> do
        newState <- revealFlop game
        outputFlop newState
        return newState

    Flop -> do
        newState <- revealTurn game
        outputTurn newState
        return newState

    Turn -> do
        newState <- revealRiver game
        outputRiver newState
        return newState

    River -> return $ game & state .~ Showdown

    _ -> error "Programming error in nextState"

nextState' :: Game -> Game
nextState' game = updatePot $ 
    game & allPlayers.madeInitialBet .~ False
         & allPlayers.canReRaise .~ True
         & playerInfo.playerTurn .~ advanceDealer game
         & bets.currentBet .~ 0
         & bets.minimumRaise .~ game^.bets.bigBlindSize
    where allPlayers = playerInfo.players.traversed

nextRound :: Game -> IO Game
nextRound game = do
    let (newState, maybeRemoved) = nextRound' game
    outputPlayersRemoved newState maybeRemoved
    return newState

nextRound' :: Game -> (Game, Maybe [Player])
nextRound' game = 
    let newState' = newState & allPlayers.inPlay .~ True
                             & allPlayers.bet .~ 0
                             & allPlayers.madeInitialBet .~ False
                             & allPlayers.hand .~ []
                             & allPlayers.handValue .~ Nothing
                             & allPlayers.canReRaise .~ True
                             & playerInfo.dealer .~ advanceDealer newState
                             & playerInfo.playerTurn .~ advancePlayerTurn 
                                                        newState
                             & state .~ PreFlop
                             & cardInfo .~ Cards [] fullDeck
                             & roundDone .~ False
                             & bets.pots .~ []
                             & bets.currentBet .~ 0
                             & bets.minimumRaise .~ newState^.bets.bigBlindSize
                             & roundNumber +~ 1
    in (newState', maybeRemoved)
    where allPlayers = playerInfo.players.traversed
          (newState, maybeRemoved) = removeOutPlayers game
