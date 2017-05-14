module Betting where

import Types
import Control.Lens

makeBet :: Int -> Int -> Game -> Game
makeBet amount player game = game & playerInfo.players.ix player.chips -~ amount
                                  & playerInfo.players.ix player.bet +~ amount

goAllIn :: Int -> Game -> Game
goAllIn player game = let newState = makeBet playerChips player game
                      in  newState & playerInfo.players.ix player.allIn .~ True
    where playerChips = game^.playerInfo.players ^?! ix player.chips

smallBlind :: Int -> Game -> Game
smallBlind player game = makeBet (game^.bets.smallBlindSize) player game

bigBlind :: Int -> Game -> Game
bigBlind player game = makeBet (game^.bets.bigBlindSize) player game
