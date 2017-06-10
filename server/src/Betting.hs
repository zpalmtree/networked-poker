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

promptBet :: Game -> Bool -> IO Game
promptBet game canCheck
    | game^.bets.currentBet >= totalChips = foldAllIn game
    | canCheck = do
        input <- foldCheckRaise game
        return $ handleInput game input
    | otherwise = do
        input <- foldCallRaise game
        return $ handleInput game input
    where totalChips = getCurrentPlayer game^.bet + getCurrentPlayer game^.chips

handleInput :: Game -> Action Int -> Game
handleInput game action = case action of
    Fold -> fold game
    Check -> game
    Call -> call game
    (Raise n) -> updateMinimumRaise (raise n game) n
    AllIn -> goAllIn game

fold :: Game -> Game
fold game = game & setCurrentPlayer game . inPlay .~ False

--if not all in -> set raise to matched - currentplayer
--if all in, check if raise was matched
raise :: Int -> Game -> Game
raise amount game
    | playerChips == amount = goAllIn game
    | otherwise = makeBet amount game
    where playerChips = getCurrentPlayer game^.chips

call :: Game -> Game
call game = raise (game^.bets.currentBet - getCurrentPlayer game^.bet) game

giveWinnings :: (Player, [Player]) -> Game -> Game
giveWinnings (winner, losers) game = game & playerInfo.players .~ newPlayers
    where newPlayer = winner & chips +~ gatherChips game
          newPlayers = sortBy (compare `on` (^.num)) $ newPlayer : losers
