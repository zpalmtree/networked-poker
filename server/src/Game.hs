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

{-
gameLoop :: Game -> IO Game
gameLoop game = do
    newGame <- nextRound =<< postBlindsAndPlayRound game
    if newGame^.gameFinished
        then return newGame
        else do
            outputRoundNumber newGame
            gameLoop =<< dealCards newGame
-}

postBlindsAndPlayRound :: GameStateT ()
postBlindsAndPlayRound = do
    smallBlind
    nextPlayer
    bigBlind
    nextPlayer
    playRound

{-
postBlindsAndPlayRound :: Game -> IO Game
postBlindsAndPlayRound game = do
    newGame <- nextPlayer <$> smallBlind game
    playRound =<< nextPlayer <$> bigBlind newGame
-}

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

    where (winner, losers) = victor (s^.playerInfo.depreciatedPlayers)
          isShowdown = case s^.stage of
                        Showdown -> True
                        _ -> False


{-
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

    where (winner, losers) = victor (game^.playerInfo.depreciatedPlayers)
          isShowdown = case game^.state of
                        Showdown -> True
                        _ -> False
-}

showdown :: GameStateT (Pot, [Player])
showdown = do
    s <- get
    getWinnersAndDistribute (s^.bets.pots)

{-
showdown :: Game -> (Game, [(Pot, [Player])])
showdown game = getWinnersAndDistribute results (results^.bets.pots) []
    where results = getHandValue game
-}

getWinnersAndDistribute :: [Pot] -> GameStateT (Pot, [Player])
getWinnersAndDistribute (p:ps) = do
    winners <- distributePot game pot
    winners' <- getWinnersAndDistribute ps
    return $ winners ++ winners'

{-
getWinnersAndDistribute :: Game -> [Pot] -> [(Pot, [Player])]
                                -> (Game, [(Pot, [Player])])
getWinnersAndDistribute game [] acc = (game, acc)
getWinnersAndDistribute game (pot:pots') acc = recurse
    where (newGame, winnerMapping) = distributePot game pot
          recurse = getWinnersAndDistribute newGame pots' (winnerMapping : acc)
-}

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

{-
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
-}

{-
nextState' :: Game -> Game
nextState' game = updatePot $ 
    game & allPlayers.madeInitialBet .~ False
         & allPlayers.canReRaise .~ True
         & playerInfo.depreciatedPlayerTurn .~ advanceDealer game
         & bets.currentBet .~ 0
         & bets.minimumRaise .~ game^.bets.bigBlindSize
    where allPlayers = playerInfo.depreciatedPlayers.traversed
-}

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

{-
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
                           & playerInfo.depreciatedDealer .~ advanceDealer newGame
                           & playerInfo.depreciatedPlayerTurn .~ advancePlayerTurn newGame
                           & state .~ PreFlop
                           & cardInfo .~ Cards [] fullDeck
                           & roundDone .~ False
                           & bets.pots .~ []
                           & bets.currentBet .~ 0
                           & bets.minimumRaise .~ newGame^.bets.bigBlindSize
                           & roundNumber +~ 1
    in (newGame', maybeRemoved)
    where allPlayers = playerInfo.depreciatedPlayers.traversed
          (newGame, maybeRemoved) = removeOutPlayers game
-}
