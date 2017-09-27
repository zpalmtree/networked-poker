{-# LANGUAGE CPP #-}

module Game
(
    gameLoop
)
where

import Control.Lens ((^.), (+~), (&), (.~), (.=), traversed, zoom)
import Control.Monad.Trans.State (StateT, get)
import Control.Monad.Trans.Class ()

import Types (Game, Pot, Player, Stage(..), Cards(..), GameStateT)
import Betting (smallBlind, bigBlind, giveWinnings, promptBet, updatePot)

import Utilities.Player (nextPlayer, numInPlay, getCurrentPlayer, numAllIn,
                         victor, advanceDealer, advancePlayerTurn,
                         removeOutPlayers)

import Utilities.Card (dealCards, revealFlop, revealTurn, revealRiver, fullDeck)
import Showdown (getHandValue, distributePot)

import Lenses (gameFinished, bet, bets, currentBet, inPlay, madeInitialBet,
               allIn, playerInfo, depreciatedPlayers, pots, stage, canReRaise, 
               depreciatedPlayerTurn, minimumRaise, bigBlindSize, hand, 
               handInfo, depreciatedDealer, cardInfo, roundDone, roundNumber)

#ifdef DEBUG
import Output.Terminal.Output (outputRoundNumber, outputHandValues, 
                               outputWinners, outputWinner, outputFlop,
                               outputTurn, outputRiver, outputPlayersRemoved)
#else
import Output.Network.Output (outputRoundNumber, outputHandValues, 
                              outputWinners, outputWinner, outputFlop,
                              outputTurn, outputRiver, outputPlayersRemoved)
#endif

gameLoop :: GameStateT ()
gameLoop = do
    postBlindsAndPlayRound
    nextRound

    s <- get

    if s^.gameFinished
        then return ()
        else do
            outputRoundNumber
            dealCards
            gameLoop

postBlindsAndPlayRound :: GameStateT ()
postBlindsAndPlayRound = do
    smallBlind
    nextPlayer
    bigBlind
    nextPlayer
    playRound

playRound :: GameStateT ()
playRound = do
    s <- get
    makeChoice s

makeChoice :: Game -> GameStateT ()
makeChoice s
    -- in showdown -> no more betting can happen
    | isShowdown = do
        winnerMapping <- showdown
        outputHandValues
        outputWinners winnerMapping

    -- only one player left -> they get the winnings, start next round
    | s^.numInPlay == 1 = do
        winner <- victorID
        outputWinner winner
        giveWinnings (winner, losers)

    -- max of one person isn't all in -> no more betting can happen -> deal
    -- more cards, but can't do anymore betting
    | s^.getCurrentPlayer.bet == s^.bets.currentBet && 
      s^.numInPlay - s^.numAllIn <= 1 = do
        nextState
        playRound

    -- player isn't in play, go onto next player 
    | not $ s^.getCurrentPlayer.inPlay = do
        nextPlayer
        playRound 

    -- player is in play, and hasn't made their initial bet, so prompt for bet
    -- they can check because their bet is equal to the current bet - i.e.
    -- they were big blind
    | not (s^.getCurrentPlayer.madeInitialBet) 
       && (s^.getCurrentPlayer.bet == s^.bets.currentBet) 
       && not (s^.getCurrentPlayer.allIn) = do
        promptBet True
        nextPlayer
        playRound

    -- player is in play, and hasn't made their intial bet, so prompt for bet
    -- isn't matched with current bet, so can't check
    | not (s^.getCurrentPlayer.madeInitialBet) &&
      not (s^.getCurrentPlayer.allIn) = do
        promptBet False
        nextPlayer
        playRound

    -- player is in play, and has made initial bet, but isn't matched with
    -- current bet level -> has to call/fold/raise
    | s^.getCurrentPlayer.madeInitialBet && 
      s^.getCurrentPlayer.bet < s^.bets^.currentBet && -- combine this with above?
      not (s^.getCurrentPlayer.allIn) = do
        promptBet False
        nextPlayer
        playRound

    -- else the player has already made their bet so move on to next load of 
    -- cards and bets
    | otherwise = do
        nextState
        playRound

    where isShowdown = case s^.stage of
                        Showdown -> True
                        _ -> False

showdown :: GameStateT (Pot, [Player])
showdown = do
    s <- get
    getWinnersAndDistribute (s^.bets.pots)

getWinnersAndDistribute :: [Pot] -> GameStateT (Pot, [Player])
getWinnersAndDistribute (p:ps) = do
    winners <- distributePot game pot
    winners' <- getWinnersAndDistribute ps
    return $ winners ++ winners'

nextState :: GameStateT ()
nextState = do
    s <- get

    zoom (playerInfo.depreciatedPlayers.traversed) $ do
        madeInitialBet .= False
        canReRaise .= True

    zoom bets $ do
        currentBet .= 0
        minimumRaise .= (s^.bets.bigBlingSize)

    playerInfo.depreciatedPlayerTurn .= advanceDealer

    updatePot
        
    case s^.stage of
        PreFlop -> do
            revealFlop
            outputFlop

        Flop -> do
            revealTurn
            outputTurn

        Turn -> do
            revealRiver
            outputRiver

        River -> do
            state .= Showdown

        Showdown -> error "Can't advance stage at showdown!"

nextRound :: GameStateT ()
nextRound = do
    s <- get

    removed <- removeOutPlayers 

    zoom (playerInfo.depreciatedPlayers.traversed) $ do
        inPlay .= True
        bet .= 0
        madeInitialBet .= False
        hand .= []
        handInfo .= Nothing
        canReRaise .= True
        allIn .= False

    zoom (playerInfo) $ do
        depreciatedDealer .= advanceDealer
        depreciatedPlayerTurn .= advancePlayerTurn

    zoom (bets) $ do
        pots .= []
        currentBet .= 0
        minimumRaise .= s^.bets.bigBlindSize

    state .= PreFlop
    cardInfo .= Cards [] fullDeck
    roundDone .= False
    roundNumber += 1

    return removed
