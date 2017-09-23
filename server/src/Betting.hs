{-# LANGUAGE CPP #-}

module Betting
(
    updatePot,
    gatherChips,
    smallBlind,
    bigBlind,
    promptBet,
    giveWinnings
)
where

import Control.Monad.Trans.State (State, get)
import Control.Lens (traversed, zoom, ix, filtered, (-=), (+=), (.=),
                     (^.), (^..), (&), (^?!), (<|), (%=), (+~), (.~))

import Data.List (partition, sortBy)
import Data.Function (on)

import Types (Game, Action(..), Player, Pot(..), GameState, GameStateT)
import Utilities.Player (getCurrentPlayer, setCurrentPlayer)
import Utilities.Types (fromPure)

import Lenses (bets, currentBet, chips, bet, smallBlindSize, bigBlindSize,
               pots, pot, playerInfo, depreciatedPlayers, inPlay, num, allIn, 
               minimumRaise, canReRaise, madeInitialBet, playerQueue, players)

#ifdef DEBUG
import Input.Terminal.Input (foldAllIn, checkAllIn, checkRaiseAllIn,
                             foldCallAllIn, foldCallRaiseAllIn)

import Output.Terminal.Output (outputSmallBlindMade, outputBigBlindMade,
                               outputPlayerTurn, outputAction)
#else
import Input.Network.Input (foldAllIn, checkAllIn, checkRaiseAllIn,
                            foldCallAllIn, foldCallRaiseAllIn)

import Output.Network.Output (outputSmallBlindMade, outputBigBlindMade,
                              outputPlayerTurn, outputAction)
#endif

makeBet :: Int -> GameState ()
makeBet amount = do
    zoom (playerQueue.players.ix 0) $ do
        chips -= amount
        bet += amount

    s <- get
    bets.currentBet .= max (s^.bets.currentBet) amount

smallBlind :: GameStateT ()
smallBlind = do
    outputSmallBlindMade
    s <- get
    fromPure $ makeBet (s^.bets.smallBlindSize)

bigBlind :: GameStateT ()
bigBlind = do
    outputBigBlindMade
    s <- get
    fromPure $ makeBet (s^.bets.bigBlindSize)

gatherChips :: GameState Int
gatherChips = do
    s <- get
    return $ sum (s^..bets.pots.traversed.pot)
           + sum (s^..playerQueue.players.traversed.bet)

updatePotSimple :: GameState ()
updatePotSimple = do
    s <- get

    let potSize = sum $ s^..playerQueue.players.traversed.bet
        inPlayers = filter (^.inPlay) (s^.playerQueue.players)
        ids = inPlayers^..traversed.num
        newPot = [Pot potSize ids]
        oldPot = head $ s^.bets.pots
        updatedPot = [Pot (oldPot^.pot + potSize) ids]
        fixedPot
            | null (s^.bets.pots) = newPot
            | otherwise = updatedPot

    playerQueue.players.traversed.bet .= 0
    bets.pots .= fixedPot

updatePot :: GameState ()
updatePot = do
    s <- get

    -- want to use nice guard notation instead of tons of nested ifs
    updatePot' s

updatePot' :: Game -> GameState ()
updatePot' game
    | sum (game^..playerQueue.players.traversed.bet) < 0 = error "Negative bets!"
    | sum (game^..playerQueue.players.traversed.bet) == 0 = return ()
    | length potEligible == 1 = refund refundPlayer
    | not $ any (^.allIn) (game^.playerQueue.players) = updatePotSimple
    | otherwise = do
        addPot sidePotSize
        updatePot

    where potEligible = filter (\p -> p^.bet > 0 && p^.inPlay)
                               (game^.playerInfo.depreciatedPlayers)
          sidePotSize = minimum $ potEligible^..traversed.bet
          refundPlayer = head potEligible^.num

refund :: Int -> GameState ()
refund n = do
    s <- get

    zoom (playerQueue.players.ix n) $ do
        chips += (s^.playerQueue.players.ix n.bet)
        bet .= 0

addPot :: Int -> GameState ()
addPot betSize = do
    s <- get

    spareChips <- takeOutPlayersPot betSize

    let eligible = filter potEligible (s^.playerQueue.players)
        potSize = spareChips + length eligible * betSize
        newPot = Pot potSize (eligible^..traversed.num)
    
    eligible'.bet -= betSize
    bets.pots %= (newPot :)

    where eligible' = playerQueue.players.traversed.(filtered potEligible)
          nonEligible = playerQueue.players.traversed.(filtered (\x -> not $ potEligible x))

potEligible p = p^.inPlay && p^.bet > 0
        
{-
addPot :: Game -> Int -> Game
addPot game betSize = game & bets.pots %~ (newPot :)
                           & playerInfo.depreciatedPlayers .~ newPlayers

    where (potEligible, rest) = partition (\p -> p^.inPlay && p^.bet > 0) 
                                          (game^.playerInfo.depreciatedPlayers)

          potSize = spareChips + length potEligible * betSize
          newBetters = potEligible & traversed.bet -~ betSize
          newPot = Pot potSize (potEligible^..traversed.num)
          newPlayers = sortBy (compare `on` (^.num)) (newBetters ++ newRest)
          (newRest, spareChips) = takeOutPlayersPot rest betSize [] 0
-}

takeOutPlayersPot :: Int -> GameState Int
takeOutPlayersPot betSize = do
    result <- zoom nonEligible $ do
        p <- get

        let numChips = if p^.bet >= betSize
                        then betSize
                        else p^.bet

        bet -= numChips

        return numChips

    return $ result -- what does this return??

    where nonEligible = playerQueue.players.traversed.(filtered (\x -> not $ potEligible x))

updateMinimumRaise :: Int -> GameState ()
updateMinimumRaise raise' = do
    bets.minimumRaise .= raise'
    playerQueue.players.traversed.canReRaise .= True

promptBet :: Bool -> GameStateT ()
promptBet canCheck = do
    s <- get

    promptBet' s canCheck

promptBet' :: Game -> Bool -> GameStateT ()
promptBet' s canCheck
    -- doesn't have enough chips to match the current bet
    -- must fold or go all in
    | s^.bets.currentBet >= totalChips = promptAndUpdate foldAllIn

    -- can check, but doesn't have enough chips to make a minimum raise, so
    -- must go all in if they want to raise
    | canCheck && cantRaise = promptAndUpdate checkAllIn

    -- matches current bet, so can check or raise
    | canCheck = promptAndUpdate checkRaiseAllIn

    -- raise hasn't been matched, so can only fold or call
    -- this is due to a player going all in with a raise lower than the minimum
    -- bet, so technically the raise hasn't been matched. Player can't re-raise
    -- in this case.
    | not (getCurrentPlayer s^.canReRaise) = promptAndUpdate foldCallAllIn 

    -- otherise it's a standard betting optino of fold/call/raise
    | otherwise = promptAndUpdate foldCallRaiseAllIn

    where totalChips = getCurrentPlayer s^.bet + getCurrentPlayer s^.chips
          cantRaise = getCurrentPlayer s^.chips < s^.bets.minimumRaise

promptAndUpdate :: (GameStateT (Action Int)) -> GameStateT ()
promptAndUpdate f = do
    outputPlayerTurn
    action <- f
    outputAction action
    fromPure $ handleInput action
    playerQueue.players.ix 0.madeInitialBet .= True

handleInput :: Action Int -> GameState ()
handleInput action = case action of
    Fold -> fold
    Check -> return ()
    Call -> call
    (Raise n) -> raise n
    AllIn -> goAllIn

fold :: GameState ()
fold = playerQueue.players.ix 0.inPlay .= False

raise :: Int -> GameState ()
raise amount = do
    s <- get

    let raise' = amount - s^.bets.currentBet
        bet' = amount - getCurrentPlayer s^.bet

    if raise' == getCurrentPlayer s^.chips 
        then goAllIn
        else updateMinimumRaise (makeBet bet') raise'

call :: GameState ()
call = do
    s <- get
    makeBet (s^.bets.currentBet - getCurrentPlayer s^.bet)

-- if it's a raise and it's at least the minimum bet, then let any previous
-- raisers re-raise, plus update minimum raise
goAllIn :: GameState ()
goAllIn = do
    s <- get

    let bet' = getCurrentPlayer s^.chips + getCurrentPlayer s^.bet
        raise' = getCurrentPlayer s^.chips

    makeBet raise'
    playerQueue.players.ix 0.allIn .= True

    if bet' > s^.bets.currentBet && raise' >= s^.bets.minimumRaise
        then updateMinimumRaise raise'
        else return ()

giveWinnings :: Int -> GameState ()
giveWinnings winnerID = do
    winnings <- gatherChips
    zoom (playerQueue.players.traversed.(filtered (\p -> p^.num == winnerID))) $ do
        chips += winnings
