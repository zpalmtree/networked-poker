module Game
(
    gameLoop
)
where

import Control.Lens ((^.), (.=), (+=), traversed, zoom)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (unless, when)
import System.Log.Logger (debugM)
import Data.IORef (readIORef)

import Betting (smallBlind, bigBlind, giveWinnings, promptBet, updatePot)
import Showdown (distributePot, calculateHandValues)
import Utilities.Card (dealCards, revealFlop, revealTurn, revealRiver)
import DrawCard (getRNGFunc, getInitFunc, initM)
import Types (Game, Pot, Stage(..), GameStateT, Player)

import Utilities.Player 
    (nextPlayer, getCurrentPlayerPure, removeOutPlayers, numInPlayPure, 
     victor, numAllInPure, nextDealer, resetDealer)

import Lenses 
    (gameFinished, bet, bets, currentBet, inPlay, madeInitialBet, allIn, pots, 
     stage, canReRaise, minimumRaise, bigBlindSize, handInfo, shuffleType,
     roundDone, roundNumber, playerQueue, players, pot, nextRoundShuffleType,
     cardInfo, tableCards, randomSource, algorithm)

import Output
    (outputHandValues, outputNewChips, outputCards, outputPlayersRemoved,
     outputResetRound, outputNextState, outputNewShuffle)

gameLoop :: GameStateT ()
gameLoop = do
    postBlindsAndPlayRound

    nextRound

    s <- get

    unless (s^.gameFinished) $ do
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
        lift $ debugM "Prog.makeChoice" "inShowdown"

        winners <- showdown
        outputHandValues
        outputNewChips winners

    | onlyOnePlayerLeft s = do
        lift $ debugM "Prog.makeChoice" "onlyOnePlayerLeft"

        winner <- victor

        updatePot

        pot' <- giveWinnings winner

        outputNewChips [(pot', [winner])]

    | maxOneNotAllIn s = do
        lift $ debugM "Prog.makeChoice" "maxOneNotAllIn"

        nextState
        playRound

    | notInPlay s = do
        lift $ debugM "Prog.makeChoice" "notInPlay"

        nextPlayer
        playRound 

    | inPlayCanCheck s = do
        lift $ debugM "Prog.makeChoice" "inPlayCanCheck"

        promptBet True
        nextPlayer
        playRound

    | notAllInAndUnmatched s = do
        lift $ debugM "Prog.makeChoice" "notAllInAndUnmatched"

        promptBet False
        nextPlayer
        playRound

    --already bet or all in
    | otherwise = do
        lift $ debugM "Prog.makeChoice" "defaultChoice"

        nextState
        playRound

showdown :: GameStateT [(Int, [Player])]
showdown = do
    calculateHandValues
    s <- get
    getWinnersAndDistribute (s^.bets.pots)

getWinnersAndDistribute :: [Pot] -> GameStateT [(Int, [Player])]
getWinnersAndDistribute [] = return []
getWinnersAndDistribute (p:ps) = do
    winners <- distributePot p
    moreWinners <- getWinnersAndDistribute ps
    return $ (p^.pot, winners) : moreWinners

nextState :: GameStateT ()
nextState = do
    s <- get

    zoom (playerQueue.players.traversed) $ do
        madeInitialBet .= False
        canReRaise .= True

    zoom bets $ do
        currentBet .= 0
        minimumRaise .= s^.bets.bigBlindSize

    resetDealer

    updatePot

    case s^.stage of
        PreFlop -> do
            revealFlop
            outputCards

        Flop -> do
            revealTurn
            outputCards

        Turn -> do
            revealRiver
            outputCards

        River -> stage .= Showdown

        Showdown -> error "Can't advance stage at showdown!"

    outputNextState

nextRound :: GameStateT ()
nextRound = do
    removeOutPlayers outputPlayersRemoved

    s <- get

    unless (s^.gameFinished) $ do
        zoom (playerQueue.players.traversed) $ do
            inPlay .= True
            bet .= 0
            madeInitialBet .= False
            handInfo .= Nothing
            canReRaise .= True
            allIn .= False

        zoom bets $ do
            pots .= []
            currentBet .= 0
            minimumRaise .= s^.bets.bigBlindSize

        stage .= PreFlop

        newShuffle <- lift $ readIORef (s^.nextRoundShuffleType)

        when (newShuffle /= s^.shuffleType) $ do
            shuffleType .= newShuffle
            outputNewShuffle

        s' <- get

        let initFunc = getInitFunc (s'^.shuffleType.algorithm)
            rngFunc = getRNGFunc (s'^.shuffleType.randomSource)

        initM initFunc rngFunc

        cardInfo.tableCards .= []

        roundDone .= False
        roundNumber += 1

        nextDealer
        resetDealer

        outputResetRound
