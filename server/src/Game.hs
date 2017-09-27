{-# LANGUAGE CPP #-}

module Game
(
    gameLoop
)
where

import Control.Lens ((^.), (.=), (+=), traversed, zoom)
import Control.Monad.Trans.State (get)
import Control.Monad (unless)

import Types (Game, Pot, Player, Stage(..), Cards(..), GameStateT)
import Betting (smallBlind, bigBlind, giveWinnings, promptBet, updatePot)
import Showdown (distributePot, calculateHandValues)
import Utilities.Types (fromPure)

import Utilities.Card 
    (dealCards, revealFlop, revealTurn, revealRiver, fullDeck)

import Utilities.Player 
    (nextPlayerT, getCurrentPlayerPure, removeOutPlayers, numInPlayPure, 
     victorID, numAllInPure, nextDealerT)

import Lenses 
    (gameFinished, bet, bets, currentBet, inPlay, madeInitialBet, allIn, pots, 
     stage, canReRaise, minimumRaise, bigBlindSize, hand, handInfo, cardInfo, 
     roundDone, roundNumber, playerQueue, players)

#ifdef DEBUG
import Output.Terminal.Output 
    (outputRoundNumber, outputHandValues, outputWinners, outputWinner, 
     outputFlop, outputTurn, outputRiver, outputPlayersRemoved)

#else
import Output.Network.Output 
    (outputRoundNumber, outputHandValues, outputWinners, outputWinner, 
     outputFlop, outputTurn, outputRiver, outputPlayersRemoved)
#endif

gameLoop :: GameStateT ()
gameLoop = do
    postBlindsAndPlayRound
    nextRound

    s <- get

    unless (s^.gameFinished) $ do
        outputRoundNumber
        dealCards
        gameLoop

postBlindsAndPlayRound :: GameStateT ()
postBlindsAndPlayRound = do
    smallBlind
    nextPlayerT
    bigBlind
    nextPlayerT
    playRound

playRound :: GameStateT ()
playRound = do
    s <- get
    makeChoice s

-- in showdown -> no more betting can happen
inShowdown :: Game -> Bool
inShowdown s = s^.stage == Showdown

-- only one player left -> they get the winnings, start next round
onlyOnePlayerLeft :: Game -> Bool
onlyOnePlayerLeft s = numInPlayPure s == 1

-- max of one person isn't all in -> no more betting can happen -> deal
-- more cards, but can't do anymore betting
maxOneNotAllIn :: Game -> Bool
maxOneNotAllIn s = getCurrentPlayerPure s^.bet == s^.bets.currentBet && 
                   numInPlayPure s - numAllInPure s <= 1

-- player isn't in play, go onto next player 
notInPlay :: Game -> Bool
notInPlay s = not $ getCurrentPlayerPure s^.inPlay

-- player is in play, and hasn't made their initial bet, so prompt for bet
-- they can check because their bet is equal to the current bet - i.e.
-- they were big blind
inPlayCanCheck :: Game -> Bool
inPlayCanCheck s = not (p^.madeInitialBet) && 
                   not (p^.allIn) &&
                   p^.bet == s^.bets.currentBet

    where p = getCurrentPlayerPure s

-- in play but unmatched bet, so prompt for bet, can't check
notAllInAndUnmatched :: Game -> Bool
notAllInAndUnmatched s = not (p^.allIn) && p^.bet < s^.bets.currentBet
    where p = getCurrentPlayerPure s

makeChoice :: Game -> GameStateT ()
makeChoice s
    | inShowdown s = do
        winnerMapping <- showdown
        outputHandValues
        outputWinners winnerMapping

    | onlyOnePlayerLeft s = do
        winner <- fromPure victorID
        outputWinner winner
        fromPure $ giveWinnings winner

    | maxOneNotAllIn s = do
        nextState
        playRound

    | notInPlay s = do
        nextPlayerT
        playRound 

    | inPlayCanCheck s = do
        promptBet True
        nextPlayerT
        playRound

    | notAllInAndUnmatched s = do
        promptBet False
        nextPlayerT
        playRound

    --already bet or all in
    | otherwise = do
        nextState
        playRound

showdown :: GameStateT [(Pot, [Player])]
showdown = do
    fromPure calculateHandValues
    s <- get
    getWinnersAndDistribute (s^.bets.pots)

getWinnersAndDistribute :: [Pot] -> GameStateT [(Pot, [Player])]
getWinnersAndDistribute [] = return []
getWinnersAndDistribute (p:ps) = do
    winners <- fromPure $ distributePot p
    winners' <- getWinnersAndDistribute ps
    return $ winners : winners'

nextState :: GameStateT ()
nextState = do
    s <- get

    zoom (playerQueue.players.traversed) $ do
        madeInitialBet .= False
        canReRaise .= True

    zoom bets $ do
        currentBet .= 0
        minimumRaise .= (s^.bets.bigBlindSize)

    nextPlayerT

    fromPure updatePot
        
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

        River -> stage .= Showdown

        Showdown -> error "Can't advance stage at showdown!"

nextRound :: GameStateT ()
nextRound = do
    s <- get

    removed <- fromPure removeOutPlayers 

    zoom (playerQueue.players.traversed) $ do
        inPlay .= True
        bet .= 0
        madeInitialBet .= False
        hand .= []
        handInfo .= Nothing
        canReRaise .= True
        allIn .= False

    zoom bets $ do
        pots .= []
        currentBet .= 0
        minimumRaise .= s^.bets.bigBlindSize

    stage .= PreFlop
    cardInfo .= Cards [] fullDeck
    roundDone .= False
    roundNumber += 1

    nextPlayerT
    nextDealerT

    outputPlayersRemoved removed
