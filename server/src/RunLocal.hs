module RunLocal
(
    run
)
where

import Types (Game)
import Game (gameLoop)
import TestStates (testPlayer1, testPlayer2, testPlayer3, testPlayer4,
                   initialGame, initialPlayers)
import Output.Terminal.Output (outputRoundNumber, outputGameOver)
import CardUtilities (dealCards)

run :: IO ()
run = play

play :: IO ()
play = do
    initial <- setup
    final <- gameLoop initial
    cleanup final

setup :: IO Game
setup = do
    let game = setup'
    outputRoundNumber game
    dealCards game

setup' :: Game
setup' = initialGame smallBlindSize' $ initialPlayers players'
    where players' = [testPlayer1, testPlayer2, testPlayer3, testPlayer4]
          smallBlindSize' = 10

cleanup :: Game -> IO ()
cleanup = outputGameOver
