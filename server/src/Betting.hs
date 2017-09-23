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

import Control.Lens hiding (Fold)
import Data.List (partition, sortBy)
import Data.Function (on)

import Types (Game, Action(..), Player, Pot(..))
import PlayerUtilities (getCurrentPlayer, setCurrentPlayer)
import Lenses (bets, currentBet, chips, bet, smallBlindSize, bigBlindSize,
               pots, pot, playerInfo, depreciatedPlayers, inPlay, num, allIn, minimumRaise,
               canReRaise, madeInitialBet)
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
makeBet :: Int -> Game -> Game
makeBet amount game
    | newBet > newGame^.bets.currentBet = newGame & bets.currentBet .~ newBet
    | otherwise = newGame
    where newGame = game & setCurrentPlayer game . chips -~ amount
                          & setCurrentPlayer game . bet +~ amount

          newBet = getCurrentPlayer newGame^.bet

smallBlind :: Game -> IO Game
smallBlind game = do
    outputSmallBlindMade game
    return $ smallBlind' game

bigBlind :: Game -> IO Game
bigBlind game = do
    outputBigBlindMade game
    return $ bigBlind' game

smallBlind' :: Game -> Game
smallBlind' game = makeBet (game^.bets.smallBlindSize) game

bigBlind' :: Game -> Game
bigBlind' game = makeBet (game^.bets.bigBlindSize) game

gatherChips :: Game -> Int
gatherChips game = sum (game^..bets.pots.traversed.pot)
                 + sum (game^..playerInfo.depreciatedPlayers.traversed.bet)

updatePotSimple :: Game -> Game
updatePotSimple game = game & playerInfo.depreciatedPlayers.traversed.bet .~ 0
                            & bets.pots .~ fixedPot
    where potSize = sum $ game^..playerInfo.depreciatedPlayers.traversed.bet
          inPlayers = filter (^.inPlay) (game^.playerInfo.depreciatedPlayers)
          ids = inPlayers^..traversed.num
          newPot = [Pot potSize ids]
          updatedPot = [Pot (oldPot^.pot + potSize) ids]
          oldPot = head $ game^.bets.pots
          fixedPot = if null (game^.bets.pots) then newPot else updatedPot

updatePot :: Game -> Game
updatePot game
    | sum (game^..playerInfo.depreciatedPlayers.traversed.bet) < 0 = error "Negative bets!"
    | sum (game^..playerInfo.depreciatedPlayers.traversed.bet) == 0 = game
    | length potEligible == 1 = refund game refundPlayer
    | not $ any (^.allIn) (game^.playerInfo.depreciatedPlayers) = updatePotSimple game
    | otherwise = updatePot $ addPot game sidePotSize
    where potEligible = filter (\p -> p^.bet > 0 && p^.inPlay)
                               (game^.playerInfo.depreciatedPlayers)
          sidePotSize = minimum $ potEligible^..traversed.bet
          refundPlayer = head potEligible^.num

refund :: Game -> Int -> Game
refund game n = game & p.chips +~ game^.playerInfo.depreciatedPlayers ^?! ix n.bet
                     & p.bet .~ 0
    where p = playerInfo.depreciatedPlayers.ix n

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

takeOutPlayersPot :: [Player] -> Int -> [Player] -> Int -> ([Player], Int)
takeOutPlayersPot [] _ players' n = (players', n)
takeOutPlayersPot (p:ps) betSize acc n
    | p^.bet >= betSize = takeOutPlayersPot ps betSize (newP : acc) 
                                                       (n + betSize)
    | otherwise = takeOutPlayersPot ps betSize (newP2 : acc) (n + p^.bet)
    where newP = p & bet -~ betSize
          newP2 = p & bet .~ 0

updateMinimumRaise :: Game -> Int -> Game
updateMinimumRaise game raise' = game & bets.minimumRaise .~ raise'
                                      & allPlayers.canReRaise .~ True
                                      & setRaiseMatched .~ False
    where allPlayers = playerInfo.depreciatedPlayers.traversed
          setRaiseMatched = setCurrentPlayer game.canReRaise

promptBet :: Game -> Bool -> IO Game
promptBet game canCheck
    -- doesn't have enough chips to match the current bet
    -- must fold or go all in
    | game^.bets.currentBet >= totalChips = promptAndUpdate foldAllIn game

    -- can check, but doesn't have enough chips to make a minimum raise, so
    -- must go all in if they want to raise
    | canCheck && cantRaise = promptAndUpdate checkAllIn game

    -- matches current bet, so can check or raise
    | canCheck = promptAndUpdate checkRaiseAllIn game

    -- raise hasn't been matched, so can only fold or call
    -- this is due to a player going all in with a raise lower than the minimum
    -- bet, so technically the raise hasn't been matched. Player can't re-raise
    -- in this case.
    | not (getCurrentPlayer game^.canReRaise) = promptAndUpdate foldCallAllIn 
                                                game

    -- otherise it's a standard betting optino of fold/call/raise
    | otherwise = promptAndUpdate foldCallRaiseAllIn game

    where totalChips = getCurrentPlayer game^.bet + getCurrentPlayer game^.chips
          cantRaise = getCurrentPlayer game^.chips < game^.bets.minimumRaise

promptAndUpdate :: (Game -> IO (Action Int)) -> Game -> IO Game
promptAndUpdate f game = do
    -- update players' GUI's on whose turn it is
    outputPlayerTurn game
    -- get the players actions
    action <- f game
    -- update the players' GUI's on what action they made
    outputAction game action
    let newGame = handleInput game action
    return $ newGame & setCurrentPlayer newGame.madeInitialBet .~ True

handleInput :: Game -> Action Int -> Game
handleInput game action = case action of
    Fold -> fold game
    Check -> game
    Call -> call game
    (Raise n) -> raise n game
    AllIn -> goAllIn game

fold :: Game -> Game
fold game = game & setCurrentPlayer game . inPlay .~ False

raise :: Int -> Game -> Game
raise amount game
    | raise' == getCurrentPlayer game^.chips = goAllIn game
    | otherwise = updateMinimumRaise (makeBet bet' game) raise'
    where raise' = amount - game^.bets.currentBet
          bet' = amount - getCurrentPlayer game^.bet

call :: Game -> Game
call game = makeBet (game^.bets.currentBet - getCurrentPlayer game^.bet) game

-- if it's a raise and it's at least the minimum bet, then let any previous
-- raisers re-raise, plus update minimum raise
goAllIn :: Game -> Game
goAllIn game
    | bet' > game^.bets.currentBet && 
      raise' >= game^.bets.minimumRaise
      = updateMinimumRaise newGame raise'
    | otherwise = newGame
    where newGame = makeBet raise' game
                  & setCurrentPlayer game . allIn .~ True
          bet' = (getCurrentPlayer game^.chips) + (getCurrentPlayer game^.bet)
          raise' = getCurrentPlayer game^.chips

giveWinnings :: (Player, [Player]) -> Game -> Game
giveWinnings (winner, losers) game = game & playerInfo.depreciatedPlayers .~ newPlayers
    where newPlayer = winner & chips +~ gatherChips game
          newPlayers = sortBy (compare `on` (^.num)) $ newPlayer : losers
