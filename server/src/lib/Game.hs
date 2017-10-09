module Game
(
    gameLoop
)
where

import Control.Lens ((^.), (.=), (+=), traversed, zoom)
import Control.Monad.Trans.State (get)
import Control.Monad (unless)
import Data.UUID.Types (UUID)

import Types (Game, Pot, Stage(..), Cards(..), GameStateT)
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
     stage, canReRaise, minimumRaise, bigBlindSize, handInfo, cardInfo, 
     roundDone, roundNumber, playerQueue, players)

import Output
    (outputHandValues, outputWinners, outputCards, outputPlayersRemoved)

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
        winnerID <- fromPure victorID

        fromPure updatePot

        let pot' = head $ s^.bets.pots
            winner' = [winnerID]

        outputWinners [(pot', winner')]

        fromPure $ giveWinnings winnerID

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

showdown :: GameStateT [(Pot, [UUID])]
showdown = do
    fromPure calculateHandValues
    s <- get
    getWinnersAndDistribute (s^.bets.pots)

getWinnersAndDistribute :: [Pot] -> GameStateT [(Pot, [UUID])]
getWinnersAndDistribute [] = return []
getWinnersAndDistribute (p:ps) = do
    winnerIDs <- fromPure $ distributePot p
    winnerIDs' <- getWinnersAndDistribute ps
    return $ (p, winnerIDs) : winnerIDs'

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
            outputCards

        Flop -> do
            revealTurn
            outputCards

        Turn -> do
            revealRiver
            outputCards

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