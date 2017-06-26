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

import Types
import PlayerUtilities
import Input.Terminal.Input
import Output.Terminal.Output

import Control.Lens hiding (Fold)
import Data.List
import Data.Function

makeBet :: Int -> Game -> Game
makeBet amount game
    | newBet > newState^.bets.currentBet = newState & bets.currentBet .~ newBet
    | otherwise = newState
    where newState = game & setCurrentPlayer game . chips -~ amount
                          & setCurrentPlayer game . bet +~ amount

          newBet = getCurrentPlayer newState^.bet

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
                 + sum (game^..playerInfo.players.traversed.bet)

updatePotSimple :: Game -> Game
updatePotSimple game = game & playerInfo.players.traversed.bet .~ 0
                            & bets.pots .~ fixedPot
    where potSize = sum $ game^..playerInfo.players.traversed.bet
          inPlayers = filter (^.inPlay) (game^.playerInfo.players)
          ids = inPlayers^..traversed.num
          newPot = [Pot potSize ids]
          updatedPot = [Pot (oldPot^.pot + potSize) ids]
          oldPot = head $ game^.bets.pots
          fixedPot = if null (game^.bets.pots) then newPot else updatedPot

updatePot :: Game -> Game
updatePot game
    | sum (game^..playerInfo.players.traversed.bet) == 0 = game
    | length betters == 1 = refund game refundPlayer
    | not $ any (^.allIn) (game^.playerInfo.players) = updatePotSimple game
    | otherwise = updatePot $ addPot game sidePotSize
    where betters = filter (\p -> p^.bet > 0 && p^.inPlay)
                           (game^.playerInfo.players)
          sidePotSize = minimum $ filter (^.inPlay) betters^..traversed.bet
          refundPlayer = head betters^.num

refund :: Game -> Int -> Game
refund game n = game & p.chips +~ game^.playerInfo.players ^?! ix n.bet
                     & p.bet .~ 0
    where p = playerInfo.players.ix n

addPot :: Game -> Int -> Game
addPot game betSize = game & bets.pots %~ (newPot :)
                           & playerInfo.players .~ newPlayers
    where (betters, rest) = partition (\p -> p^.bet > 0 && p^.inPlay) 
                                      (game^.playerInfo.players)
          potSize = length betters * betSize
          newBetters = betters & traversed.bet -~ betSize
          newPot = Pot potSize (betters^..traversed.num)
          newPlayers = sortBy (compare `on` (^.num)) (newBetters ++ rest)

updateMinimumRaise :: Game -> Int -> Game
updateMinimumRaise game raise' = game & bets.minimumRaise .~ raise'
                                      & allPlayers.canReRaise .~ True
                                      & setRaiseMatched .~ False
    where allPlayers = playerInfo.players.traversed
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
    let newState = handleInput game action
    return $ newState & setCurrentPlayer newState.madeInitialBet .~ True

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
giveWinnings (winner, losers) game = game & playerInfo.players .~ newPlayers
    where newPlayer = winner & chips +~ gatherChips game
          newPlayers = sortBy (compare `on` (^.num)) $ newPlayer : losers
