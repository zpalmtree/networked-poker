module Main
(
    main
)
where

import Types
import Betting
import PlayerUtilities
import CardUtilities
import StateUtilities
import Showdown

import Control.Monad
import Control.Lens hiding (Fold)
import Data.List
import Data.Function

main :: IO ()
main = play

play :: IO ()
play = do
    initial <- setup
    final <- gameLoop initial
    void $ cleanup final

gameLoop :: Game -> IO Game
gameLoop game = do
    newGame <- playRound' game
    if newGame^.gameFinished
        then return game
        else gameLoop newGame

setup :: IO Game
setup = undefined

cleanup :: Game -> IO ()
cleanup = undefined

playRound' :: Game -> IO Game
playRound' game = playRound . bigBlind . nextPlayer $ smallBlind game

playRound :: Game -> IO Game
playRound game
    -- in showdown -> no more betting can happen
    | isShowdown game = return . nextRound $ showdown game
    -- only one player left -> they get the winnings, start next round
    | numInPlay game == 1 = return . nextRound $ giveWinnings winner game
    {- max of one person isn't all in -> no more betting can happen -> deal
    more cards, but can't do anymore betting -}
    | numInPlay game - numAllIn game <= 1 = playRound =<< nextState game
    -- player isn't in play, go onto next player 
    | not $ getCurrentPlayer game^.inPlay = playRound $ nextPlayer game 
    -- player is in play, and hasn't made their initial bet, so prompt for bet
    | not $ getCurrentPlayer game^.madeInitialBet = playRound . nextPlayer
                                                    =<< promptBet game True
    {- player is in play, and has made initial bet, but isn't matched with
    current bet level -> has to call/fold/raise -}
    | getCurrentPlayer game^.madeInitialBet && 
      getCurrentPlayer game^.bet < game^.bets.currentBet
        = playRound . nextPlayer =<< promptBet game False
    {- else the player has already made their bet so move on to next load of 
    cards and bets -}
    | otherwise = playRound =<< nextState game
    where winner = victor (game^.playerInfo.players)

promptBet :: Game -> Bool -> IO Game
promptBet game canCheck
    | canCheck = do
        input <- foldCheckRaise game
        return $ handleInput game input
    | otherwise = do
        input <- foldCallRaise game
        return $ handleInput game input

handleInput :: Game -> Action Int -> Game
handleInput game action = case action of
    Fold -> fold game
    Check -> game
    Call -> call game
    (Raise raiseAmount) -> raise raiseAmount game
    AllIn -> goAllIn game

{- these will get input from the network later on... -}
foldCheckRaise :: Game -> IO (Action a)
foldCheckRaise = undefined

foldCallRaise :: Game -> IO (Action a)
foldCallRaise = undefined

fold :: Game -> Game
fold game = game & setCurrentPlayer game . inPlay .~ False

{- obviously we wouldn't actually crash the server here, this error will
be propagated to the client later on -}
raise :: Int -> Game -> Game
raise amount game
    | playerChips < amount = error "Not enough chips!"
    | playerChips == amount = goAllIn game
    | otherwise = makeBet amount game
    where playerChips = getCurrentPlayer game^.chips

call :: Game -> Game
call game = raise (game^.bets.currentBet) game

{-
showdown :: Game -> Game
showdown game = uncurry (distributeWinnings game) $ getWinners game
-}

showdown :: Game -> Game
showdown game = foldl distributePot results (results^.bets.pots)
    where results = getHandValue game

nextState :: Game -> IO Game
nextState game = case game^.state of
    PreFlop -> revealFlop $ updatePot game
    Flop -> revealTurn $ updatePot game
    Turn -> revealRiver $ updatePot game
    River -> return . updatePot $ game & state .~ Showdown
    _ -> error "Programming error in nextState"

giveWinnings :: (Player, [Player]) -> Game -> Game
giveWinnings (winner, losers) game = game & playerInfo.players .~ newPlayers
    where newPlayer = winner & chips +~ gatherChips game
          newPlayers = sortBy (compare `on` (^.num)) $ newPlayer : losers

nextRound :: Game -> Game
nextRound game = newState & allPlayers.cards .~ []
                          & allPlayers.inPlay .~ True
                          & allPlayers.bet .~ 0
                          & allPlayers.hand .~ []
                          & allPlayers.handValue .~ Nothing
                          & playerInfo.dealer .~ advanceDealer newState
                          & playerInfo.playerTurn .~ advancePlayerTurn newState
                          & state .~ PreFlop
                          & cardInfo .~ Cards [] fullDeck
                          & roundDone .~ False
                          & bets.pots .~ []
                          & bets.currentBet .~ 0
    where allPlayers = playerInfo.players.traversed
          newState = removeOutPlayers game
