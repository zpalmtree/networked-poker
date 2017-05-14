module Betting where

import Types
import Control.Lens
import PlayerUtilities

makeBet :: Int -> Game -> Game
makeBet amount game = game & mutateCurrentPlayer game . chips -~ amount
                           & mutateCurrentPlayer game . bet +~ amount

goAllIn :: Game -> Game
goAllIn game = makeBet (getCurrentPlayer game^.chips) game
                & mutateCurrentPlayer game . allIn .~ True

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
