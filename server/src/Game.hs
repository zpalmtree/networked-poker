{-# LANGUAGE CPP #-}

module Game
(
    gameLoop
)
where

import Types (Game, Pot, Player, State(..), Cards(..))
import Betting (smallBlind, bigBlind, giveWinnings, promptBet, updatePot)
import PlayerUtilities (nextPlayer, numInPlay, getCurrentPlayer, numAllIn,
                        victor, advanceDealer, advancePlayerTurn,
                        removeOutPlayers)
import CardUtilities (dealCards, revealFlop, revealTurn, revealRiver, fullDeck)
import Showdown (getHandValue, distributePot)
import Lenses (gameFinished, bet, bets, currentBet, inPlay, madeInitialBet,
               allIn, playerInfo, players, pots, state, canReRaise, playerTurn,
               minimumRaise, bigBlindSize, hand, handInfo, dealer, cardInfo,
               roundDone, roundNumber)
#ifdef DEBUG
import Output.Terminal.Output (outputRoundNumber, outputHandValues, 
                               outputWinners, outputWinner, outputFlop,
                               outputTurn, outputRiver, outputPlayersRemoved)
#else
import Output.Network.Output (outputRoundNumber, outputHandValues, 
                              outputWinners, outputWinner, outputFlop,
                              outputTurn, outputRiver, outputPlayersRemoved)
#endif
import Control.Lens

gameLoop :: Game -> IO Game
gameLoop game = do
    newGame <- nextRound =<< playRound' game
    if newGame^.gameFinished
        then return newGame
        else do
            outputRoundNumber newGame
            gameLoop =<< dealCards newGame

playRound' :: Game -> IO Game
playRound' game = do
    newGame <- nextPlayer <$> smallBlind game
    playRound =<< nextPlayer <$> bigBlind newGame

playRound :: Game -> IO Game
playRound game
    -- in showdown -> no more betting can happen
    | isShowdown = do
        let (newGame, winnerMapping) = showdown game
        outputHandValues newGame
        outputWinners newGame winnerMapping
        return newGame

    -- only one player left -> they get the winnings, start next round
    | numInPlay game == 1 = do
        outputWinner game winner
        return $ giveWinnings (winner, losers) game

    -- max of one person isn't all in -> no more betting can happen -> deal
    -- more cards, but can't do anymore betting
    | getCurrentPlayer game^.bet == game^.bets.currentBet &&
      numInPlay game - numAllIn game <= 1
      = playRound =<< nextState game

    -- player isn't in play, go onto next player 
    | not $ getCurrentPlayer game^.inPlay = playRound $ nextPlayer game 

    -- player is in play, and hasn't made their initial bet, so prompt for bet
    -- they can check because their bet is equal to the current bet - i.e.
    -- they were big blind
    | not (getCurrentPlayer game^.madeInitialBet) 
       && (getCurrentPlayer game^.bet == game^.bets.currentBet) 
       && not (getCurrentPlayer game^.allIn)
      = playRound . nextPlayer =<< promptBet game True

    -- player is in play, and hasn't made their intial bet, so prompt for bet
    -- isn't matched with current bet, so can't check
    | not (getCurrentPlayer game^.madeInitialBet) &&
      not (getCurrentPlayer game^.allIn)
      = playRound . nextPlayer =<< promptBet game False

    -- player is in play, and has made initial bet, but isn't matched with
    -- current bet level -> has to call/fold/raise
    | getCurrentPlayer game^.madeInitialBet && 
      getCurrentPlayer game^.bet < game^.bets.currentBet &&
      not (getCurrentPlayer game^.allIn)
      = playRound . nextPlayer =<< promptBet game False

    -- else the player has already made their bet so move on to next load of 
    -- cards and bets
    | otherwise = playRound =<< nextState game

    where (winner, losers) = victor (game^.playerInfo.players)
          isShowdown = case game^.state of
                        Showdown -> True
                        _ -> False

showdown :: Game -> (Game, [(Pot, [Player])])
showdown game = getWinnersAndDistribute results (results^.bets.pots) []
    where results = getHandValue game

getWinnersAndDistribute :: Game -> [Pot] -> [(Pot, [Player])]
                                -> (Game, [(Pot, [Player])])
getWinnersAndDistribute game [] acc = (game, acc)
getWinnersAndDistribute game (pot:pots') acc = recurse
    where (newGame, winnerMapping) = distributePot game pot
          recurse = getWinnersAndDistribute newGame pots' (winnerMapping : acc)

nextState :: Game -> IO Game
nextState game = case newGame^.state of
    PreFlop -> do
        newGame' <- revealFlop newGame
        outputFlop newGame'
        return newGame'

    Flop -> do
        newGame' <- revealTurn newGame
        outputTurn newGame'
        return newGame'

    Turn -> do
        newGame' <- revealRiver newGame
        outputRiver newGame'
        return newGame'

    River -> return $ newGame & state .~ Showdown

    _ -> error "Programming error in nextState"
    where newGame = nextState' game

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
    let (newGame, maybeRemoved) = nextRound' game
    outputPlayersRemoved newGame maybeRemoved
    return newGame

nextRound' :: Game -> (Game, Maybe [Player])
nextRound' game = 
    let newGame' = newGame & allPlayers.inPlay .~ True
                           & allPlayers.bet .~ 0
                           & allPlayers.madeInitialBet .~ False
                           & allPlayers.hand .~ []
                           & allPlayers.handInfo .~ Nothing
                           & allPlayers.canReRaise .~ True
                           & allPlayers.allIn .~ False
                           & playerInfo.dealer .~ advanceDealer newGame
                           & playerInfo.playerTurn .~ advancePlayerTurn newGame
                           & state .~ PreFlop
                           & cardInfo .~ Cards [] fullDeck
                           & roundDone .~ False
                           & bets.pots .~ []
                           & bets.currentBet .~ 0
                           & bets.minimumRaise .~ newGame^.bets.bigBlindSize
                           & roundNumber +~ 1
    in (newGame', maybeRemoved)
    where allPlayers = playerInfo.players.traversed
          (newGame, maybeRemoved) = removeOutPlayers game
