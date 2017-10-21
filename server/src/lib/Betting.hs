module Betting
(
    updatePot,
    smallBlind,
    bigBlind,
    promptBet,
    giveWinnings
)
where

import Data.Monoid (Sum(..), getSum)

import Types (Game, Action(..), Pot(..), Player, GameState, GameStateT)
import Control.Monad.Trans.State (get)
import Control.Monad (when)
import Safe (headNote)
import Data.UUID.Types (UUID)

import Output (outputPlayerTurn, outputAction, outputGatherChips)

import Utilities.Player
    (getCurrentPlayer, getCurrentPlayerPure, getCurrentPlayer)

import Control.Lens 
    (traversed, zoom, ix, filtered, (-=), (+=), (.=), (^.), (^..), (%=))

import Lenses 
    (bets, currentBet, chips, bet, smallBlindSize, bigBlindSize, pots, pot, 
     inPlay, uuid, allIn, minimumRaise, canReRaise, madeInitialBet, playerQueue, 
     players)

import Input
    (foldAllIn, checkAllIn, checkRaiseAllIn, foldCallAllIn, foldCallRaiseAllIn)

makeBet :: (Monad m) => Int -> GameState m ()
makeBet amount = do
    when (amount < 0) $ error "Negative bet in makeBet!"

    zoom (playerQueue.players.ix 0) $ do
        chips -= amount
        bet += amount

    player <- getCurrentPlayer

    updateMaxBet (player^.bet)

updateMaxBet :: (Monad m) => Int -> GameState m ()
updateMaxBet n = do
    s <- get
    bets.currentBet .= max (s^.bets.currentBet) n

smallBlind :: GameStateT ()
smallBlind = do
    outputAction SmallBlind
    s <- get
    makeBet (s^.bets.smallBlindSize)

bigBlind :: GameStateT ()
bigBlind = do
    outputAction BigBlind
    s <- get
    makeBet (s^.bets.bigBlindSize)

gatherChips :: (Monad m) => GameState m Int
gatherChips = do
    s <- get
    return $ sum (s^..bets.pots.traversed.pot)
           + sum (s^..playerQueue.players.traversed.bet)

updatePotSimple :: (Monad m) => GameState m ()
updatePotSimple = do
    s <- get

    let potSize = sum $ s^..playerQueue.players.traversed.bet
        inPlayers = filter (^.inPlay) (s^.playerQueue.players)
        ids = inPlayers^..traversed.uuid
        newPot = [Pot potSize ids]
        oldPot = headNote "in updatePotSimple!" (s^.bets.pots)
        updatedPot = [Pot (oldPot^.pot + potSize) ids]
        fixedPot
            | null (s^.bets.pots) = newPot
            | otherwise = updatedPot

    playerQueue.players.traversed.bet .= 0
    bets.pots .= fixedPot

updatePot :: GameStateT ()
updatePot = do
    s <- get

    -- want to use nice guard notation instead of tons of nested ifs
    updatePot' s

    outputGatherChips

updatePot' :: Game -> GameStateT ()
updatePot' s
    | null eligible = return ()
    | sum (s^..playerQueue.players.traversed.bet) < 0 = error "Negative bets!"
    | sum (s^..playerQueue.players.traversed.bet) == 0 = return ()
    | length eligible == 1 = refund refundPlayer
    | not $ any (^.allIn) (s^.playerQueue.players) = updatePotSimple
    | otherwise = do
        addPot sidePotSize
        updatePot

    where eligible = filter potEligible (s^.playerQueue.players)
          sidePotSize = minimum $ eligible^..traversed.bet
          refundPlayer = headNote "in updatePot'!" eligible^.uuid

refund :: (Monad m) => UUID -> GameState m ()
refund uuid' = zoom eligible $ do
    p <- get
    chips += p^.bet
    bet .= 0
    where hasUUID p = p^.uuid == uuid'
          eligible = playerQueue.players.traversed.filtered hasUUID

addPot :: (Monad m) => Int -> GameState m ()
addPot betSize = do
    s <- get

    spareChips <- takeOutPlayersPot betSize

    let eligible = filter potEligible (s^.playerQueue.players)
        potSize = spareChips + length eligible * betSize
        newPot = Pot potSize (eligible^..traversed.uuid)
    
    eligible'.bet -= betSize
    bets.pots %= (newPot :)

    where eligible' = playerQueue.players.traversed.filtered potEligible

potEligible :: Player -> Bool
potEligible p = p^.inPlay && p^.bet > 0
        
takeOutPlayersPot :: (Monad m) => Int -> GameState m Int
takeOutPlayersPot betSize = do
    result <- zoom nonEligible $ do
        p <- get

        let uuidChips = if p^.bet >= betSize
                        then betSize
                        else p^.bet

        bet -= uuidChips

        return (Sum uuidChips)

    return $ getSum result

    where nonEligible = playerQueue.players.traversed.filtered 
                        (not . potEligible)

updateMinimumRaise :: (Monad m) => Int -> GameState m ()
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

    handleInput action

    playerQueue.players.ix 0.madeInitialBet .= True

convertMaxRaise :: Action Int -> GameStateT (Action Int)
convertMaxRaise a = do
    p <- getCurrentPlayer
    case a of
        Raise n -> if (n - p^.bet) == p^.chips
                    then return AllIn
                    else return a
        _ -> return a

handleInput :: (Monad m) => Action Int -> GameState m ()
handleInput action = case action of
    Fold -> fold
    Check -> return ()
    Call -> call
    (Raise n) -> raise n
    AllIn -> goAllIn
    _ -> error "Invalid input given in handleInput!"

fold :: (Monad m) => GameState m ()
fold = playerQueue.players.ix 0.inPlay .= False

raise :: (Monad m) => Int -> GameState m ()
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

call :: (Monad m) => GameState m ()
call = do
    s <- get
    player <- getCurrentPlayer
    makeBet (s^.bets.currentBet - player^.bet)

-- if it's a raise and it's at least the minimum bet, then let any previous
-- raisers re-raise, plus update minimum raise
goAllIn :: (Monad m) => GameState m ()
goAllIn = do
    s <- get

    player <- getCurrentPlayer

    let bet' = player^.chips + player^.bet
        raise' = player^.chips

    makeBet raise'
    playerQueue.players.ix 0.allIn .= True

    when (bet' > s^.bets.currentBet && raise' >= s^.bets.minimumRaise) $
        updateMinimumRaise raise'

giveWinnings :: (Monad m) => UUID -> GameState m ()
giveWinnings winnerUUID = do
    winnings <- gatherChips
    playerQueue.players.traversed.filtered isWinner.chips += winnings
    where isWinner p = p^.uuid == winnerUUID
