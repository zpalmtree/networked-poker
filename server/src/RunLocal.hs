module RunLocal
(
    run
)
where

import Control.Monad.Trans.State (StateT, evalStateT)

import Types (Game)
import Game (gameLoop)

import TestStates (testGame)

import Output.Terminal.Output (outputRoundNumber, outputGameOver)
import CardUtilities (dealCards)

run :: IO ()
run = evalStateT play initialGame

play :: GameStateT ()
play = do
    setup

    gameLoop

    cleanup

setup :: GameStateT ()
setup = do
    outputRoundNumber
    dealCards

cleanup :: GameStateT ()
cleanup = outputGameOver
