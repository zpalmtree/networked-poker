module RunLocal
(
    run
)
where

import Control.Monad.Trans.State (evalStateT)

import Types (GameStateT)
import Game (gameLoop)
import TestStates (testGame)
import Output.Terminal.Output (outputRoundNumber, outputGameOver)
import Utilities.Card (dealCards)

run :: IO ()
run = evalStateT play testGame

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
