module RunLocal
(
    run
)
where

import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Class ()

import Types (Game)
import Game (gameLoop)

import TestStates (testPlayer1, testPlayer2, testPlayer3, testPlayer4,
                   initialGame, initialPlayers, initialPlayerQueue)

import Output.Terminal.Output (outputRoundNumber, outputGameOver)
import CardUtilities (dealCards)

run :: IO ()
run = evalStateT play initialState

play :: StateT Game IO ()
play = do
    setup

    gameLoop

    cleanup

setup :: StateT Game IO ()
setup = do
    outputRoundNumber
    dealCards

initialState :: Game
initialState = initialGame smallBlindSize' (initialPlayers players') (initialPlayerQueue players')
    where players' = [testPlayer1, testPlayer2, testPlayer3, testPlayer4]
          smallBlindSize' = 10

cleanup :: StateT Game IO ()
cleanup = outputGameOver
