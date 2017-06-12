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
import Input.Terminal

import Control.Lens hiding (Fold)
import Data.List
import Data.Function

makeBet :: Int -> Game -> Game
makeBet amount game
    | newBet > (newState^.bets.currentBet)
      = newState & bets.currentBet .~ newBet
    | otherwise = newState
    where newState = game & setCurrentPlayer game . chips -~ amount
                          & setCurrentPlayer game . bet +~ amount

          newBet = getCurrentPlayer newState^.bet

smallBlind :: Game -> Game
smallBlind game = makeBet (game^.bets.smallBlindSize) game

bigBlind :: Game -> Game
bigBlind game = makeBet (game^.bets.bigBlindSize) game

gatherChips :: Game -> Int
gatherChips game = sum (game^..bets.pots.traversed.pot)
                 + sum (game^..playerInfo.players.traversed.bet)

updatePotSimple :: Game -> Game
updatePotSimple game
    | null $ game^.bets.pots = game & bets.pots .~ newPot
    | otherwise = game & bets.pots .~ updatedPot
    where allBets = playerInfo.players.traversed.bet
          potSize = sum $ game^..allBets
          inPlayers = filter (^.inPlay) (game^.playerInfo.players)
          ids = inPlayers^..traversed.num
          newPot = [Pot potSize ids]
          oldPot = head $ game^.bets.pots
          updatedPot = [Pot (oldPot^.pot + potSize) ids]

updatePot :: Game -> Game
updatePot game
    | sum (game^..playerInfo.players.traversed.bet) == 0 = game
    | null betters = game
    | length betters == 1 = refund game refundPlayer
    | not $ any (^.allIn) (game^.playerInfo.players) = updatePotSimple game
    | otherwise = updatePot $ addPot game sidePotSize
    where betters = filter (\p -> p^.bet > 0 && p^.inPlay)
                           (game^.playerInfo.players)
          sidePotSize = minimum $ filter (^.allIn) betters^..traversed.bet
          refundPlayer = head betters^.num

refund :: Game -> Int -> Game
refund game n = game & p.chips +~ game^.playerInfo.players ^?! ix n.bet
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
    {- doesn't have enough chips to match the current bet
       must fold or go all in -}
    | game^.bets.currentBet >= totalChips = do
        input <- foldAllIn game
        return $ handleInput game input

    {- can check, but doesn't have enough chips to make a minimum raise, so
       must go all in if they want to raise -}
    | canCheck && getCurrentPlayer game^.chips < game^.bets.minimumRaise = do
        input <- checkAllIn game
        return $ handleInput game input

    {- matches current bet, so can check or raise -}
    | canCheck = do
        input <- checkRaiseAllIn game
        return $ handleInput game input

    {- raise hasn't been matched, so can only fold or call
       this is due to a player going all in with a raise lower than the minimum
       bet, so technically the raise hasn't been matched. Player can't re-raise
       in this case. -}
    | not (getCurrentPlayer game^.canReRaise) = do
        input <- foldCallAllIn game
        return $ handleInput game input

    {- otherise it's a standard betting optino of fold/call/raise -}
    | otherwise = do
        input <- foldCallRaiseAllIn game
        return $ handleInput game input

    where totalChips = getCurrentPlayer game^.bet + getCurrentPlayer game^.chips

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
    | amount == getCurrentPlayer game^.chips = goAllIn game
    | otherwise = updateMinimumRaise (makeBet amount game) amount

call :: Game -> Game
call game = makeBet (game^.bets.currentBet - getCurrentPlayer game^.bet) game

{- if it's a raise and it's greater than the minimum bet, then let any previous
   raisers re-raise, plus update minimum raise -}
goAllIn :: Game -> Game
goAllIn game
    | bet' > game^.bets.currentBet && raise' > game^.bets.minimumRaise
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
