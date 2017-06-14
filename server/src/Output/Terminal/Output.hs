module Output.Terminal.Output
(
    outputAction,
    outputPlayerTurn,
    outputFlop,
    outputTurn,
    outputRiver,
    outputPlayerCards,
    outputWinner,
    outputWinners,
    outputGameOver,
    outputPlayersRemoved
)
where

import Types

outputAction :: Game -> Action Int -> IO ()
outputAction = undefined

outputPlayerTurn :: Game -> IO ()
outputPlayerTurn = undefined

outputFlop :: Game -> IO ()
outputFlop = undefined

outputTurn :: Game -> IO ()
outputTurn = undefined

outputRiver :: Game -> IO ()
outputRiver = undefined

outputPlayerCards :: Game -> IO ()
outputPlayerCards = undefined

outputWinner :: Game -> Player -> IO ()
outputWinner = undefined

outputWinners :: Game -> [(Pot, [Player])] -> IO ()
outputWinners = undefined

outputGameOver :: Game -> IO ()
outputGameOver = undefined

outputPlayersRemoved :: Game -> Maybe [Player] -> IO ()
outputPlayersRemoved = undefined
