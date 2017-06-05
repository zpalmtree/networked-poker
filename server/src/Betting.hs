module Betting where

import Types
import PlayerUtilities

import Control.Lens

makeBet :: Int -> Game -> Game
makeBet amount game = game & setCurrentPlayer game . chips -~ amount
                           & setCurrentPlayer game . bet +~ amount

goAllIn :: Game -> Game
goAllIn game = makeBet (getCurrentPlayer game^.chips) game
             & setCurrentPlayer game . allIn .~ True

smallBlind :: Game -> Game
smallBlind game = makeBet (game^.bets.smallBlindSize) game

bigBlind :: Game -> Game
bigBlind game = makeBet (game^.bets.bigBlindSize) game

gatherChips :: Game -> Int
gatherChips game = game^.bets.pot
                 + sum (game^..playerInfo.players.traversed.bet)

updatePot :: Game -> Game
updatePot game = game & bets.pot +~ gatherChips game
                      & bets.currentBet .~ 0
                      & playerInfo.players.traversed.bet .~ 0
