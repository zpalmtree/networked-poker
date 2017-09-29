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

import Data.Monoid (Sum(..), getSum)

import Types (Game, Action(..), Pot(..), Player, GameState, GameStateT)
import Utilities.Types (fromPure)
import Control.Monad.Trans.State (get)
import Control.Monad (when)
import Safe (headNote)

import Utilities.Player
    (getCurrentPlayer, getCurrentPlayerPure, getCurrentPlayerT)

import Control.Lens 
    (traversed, zoom, ix, filtered, (-=), (+=), (.=), (^.), (^..), (%=))

import Lenses 
    (bets, currentBet, chips, bet, smallBlindSize, bigBlindSize, pots, pot, 
     inPlay, num, allIn, minimumRaise, canReRaise, madeInitialBet, playerQueue, 
     players)

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
    when (amount < 0) $ error "Negative bet in makeBet!"

    zoom (playerQueue.players.ix 0) $ do
        chips -= amount
        bet += amount

    player <- getCurrentPlayer

    updateMaxBet (player^.bet)

updateMaxBet :: Int -> GameState ()
updateMaxBet n = do
    s <- get
    bets.currentBet .= max (s^.bets.currentBet) n

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
        oldPot = headNote "in updatePotSimple!" (s^.bets.pots)
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
updatePot' s
    | length eligible == 0 = return ()
    | sum (s^..playerQueue.players.traversed.bet) < 0 = error "Negative bets!"
    | sum (s^..playerQueue.players.traversed.bet) == 0 = return ()
    | length eligible == 1 = refund refundPlayer
    | not $ any (^.allIn) (s^.playerQueue.players) = updatePotSimple
    | otherwise = do
        addPot sidePotSize
        updatePot

    where eligible = filter potEligible (s^.playerQueue.players)
          sidePotSize = minimum $ eligible^..traversed.bet
          refundPlayer = headNote "in updatePot'!" eligible^.num

refund :: Int -> GameState ()
refund n = zoom (playerQueue.players.ix n) $ do
    p <- get
    chips += p^.bet
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

    where eligible' = playerQueue.players.traversed.filtered potEligible

potEligible :: Player -> Bool
potEligible p = p^.inPlay && p^.bet > 0
        
takeOutPlayersPot :: Int -> GameState Int
takeOutPlayersPot betSize = do
    result <- zoom nonEligible $ do
        p <- get

        let numChips = if p^.bet >= betSize
                        then betSize
                        else p^.bet

        bet -= numChips

        return (Sum numChips)

    return $ getSum result -- what does this return??

    where nonEligible = playerQueue.players.traversed.filtered 
                        (not . potEligible)

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
    | not (player^.canReRaise) = promptAndUpdate foldCallAllIn 

    -- otherise it's a standard betting optino of fold/call/raise
    | otherwise = promptAndUpdate foldCallRaiseAllIn

    where player = getCurrentPlayerPure s
          totalChips = player^.bet + player^.chips
          cantRaise = player^.chips < s^.bets.minimumRaise

promptAndUpdate :: GameStateT (Action Int) -> GameStateT ()
promptAndUpdate f = do
    outputPlayerTurn

    action <- convertMaxRaise =<< f

    outputAction action

    fromPure $ handleInput action

    playerQueue.players.ix 0.madeInitialBet .= True

convertMaxRaise :: Action Int -> GameStateT (Action Int)
convertMaxRaise a = do
    p <- getCurrentPlayerT
    case a of
        Raise n -> if (n - p^.bet) == p^.chips
                    then return AllIn
                    else return a
        _ -> return a

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

    player <- getCurrentPlayer

    let raise' = amount - s^.bets.currentBet
        bet' = amount - player^.bet

    if bet' == player^.chips 
        then goAllIn
        else do
            makeBet bet'
            updateMinimumRaise raise'

call :: GameState ()
call = do
    s <- get
    player <- getCurrentPlayer
    makeBet (s^.bets.currentBet - player^.bet)

-- if it's a raise and it's at least the minimum bet, then let any previous
-- raisers re-raise, plus update minimum raise
goAllIn :: GameState ()
goAllIn = do
    s <- get

    player <- getCurrentPlayer

    let bet' = player^.chips + player^.bet
        raise' = player^.chips

    makeBet raise'
    playerQueue.players.ix 0.allIn .= True

    when (bet' > s^.bets.currentBet && raise' >= s^.bets.minimumRaise) $
        updateMinimumRaise raise'

giveWinnings :: Int -> GameState ()
giveWinnings winnerID = do
    winnings <- gatherChips
    playerQueue.players.traversed.filtered isWinner.chips += winnings
    where isWinner p = p^.num == winnerID
