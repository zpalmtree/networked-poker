module Main where

import Types
import Control.Lens
import Betting

main :: IO ()
main = return ()

action :: Game -> Game
action game
    | game^.roundDone = game
    | otherwise = case game^.state of
        PreFlop -> action $ preflop game
        Flop -> action $ flop game
        Turn -> action $ turn game
        River -> action $ river game
        Showdown -> showdown game

preflop :: Game -> Game
preflop game = undefined

flop :: Game -> Game
flop = undefined

turn :: Game -> Game
turn = undefined

river :: Game -> Game
river = undefined

showdown :: Game -> Game
showdown = undefined

repeatN :: Int -> (Game -> IO Game) -> Game -> IO Game
repeatN 0 _ game = return game
repeatN n f game = do
    newGame <- f game
    repeatN (n-1) f newGame

initialBets :: Game -> IO Game
initialBets game = repeatN (game^.playerInfo.numPlayers) bettingRound game

bettingRound :: Game -> IO Game
bettingRound game
    | not $ currentPlayer^.inPlay = go (\x -> return (id x))
    | currentPlayer^.bet >= game^.bets.currentBet = go foldCheckRaise
    | otherwise = go foldCallRaise 
    where currentPlayer = game^.playerInfo.players ^?! ix player
          player = game^.playerInfo.playerTurn
          go f = do
            newState <- f game
            return $ nextPlayer newState

foldCheckRaise :: Game -> IO Game
foldCheckRaise game = do
    putStrLn "fold, check, or raise?"
    input <- getLine
    case input of
        "fold" -> return $ fold game
        "check" -> return game
        {- this will actually be input from the network, so we'll do some fancy
        parsing, for now just passing a fixed value to test -}
        "raise" -> return $ raise 100 game
        _ -> putStrLn "bad input" >> foldCheckRaise game


foldCallRaise :: Game -> IO Game
foldCallRaise game = do
    putStrLn "fold, call, or raise?"
    input <- getLine
    case input of
        "fold" -> return $ fold game
        "call" -> return $ call game
        "raise" -> return $ raise 100 game
        _ -> putStrLn "bad input" >> foldCheckRaise game

fold :: Game -> Game
fold game = game & playerInfo.players.ix player.inPlay .~ False
    where player = game^.playerInfo.playerTurn

raise :: Int -> Game -> Game
raise amount game
    | playerChips < amount = error "Not enough chips!"
    | playerChips == amount = goAllIn player game
    | otherwise = makeBet amount player game
    where player = game^.playerInfo.playerTurn
          playerChips = game^.playerInfo.players ^?! ix player.chips

call :: Game -> Game
call = undefined

nextPlayer :: Game -> Game
nextPlayer game = game & playerInfo.playerTurn .~ next
    where currentPlayer = game^.playerInfo.playerTurn
          numPlayers' = game^.playerInfo.numPlayers
          next = (currentPlayer + 1) `rem` numPlayers'
