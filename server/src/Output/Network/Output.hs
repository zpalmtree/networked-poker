module Output.Network.Output
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
    outputPlayersRemoved,
    outputHandValues,
    outputRoundNumber,
    outputSmallBlindMade,
    outputBigBlindMade
)
where

import Data.UUID.Types (UUID)

import Types (GameStateT, Action, Player, Pot)

outputAction :: Action Int -> GameStateT ()
outputAction = undefined

outputPlayerTurn :: GameStateT ()
outputPlayerTurn = undefined

outputFlop :: GameStateT ()
outputFlop = undefined

outputTurn :: GameStateT ()
outputTurn = undefined

outputRiver :: GameStateT ()
outputRiver = undefined

outputPlayerCards :: GameStateT ()
outputPlayerCards = undefined

outputWinner :: UUID -> GameStateT ()
outputWinner = undefined

outputWinners :: [(Pot, [Player])] -> GameStateT ()
outputWinners = undefined

outputGameOver :: GameStateT ()
outputGameOver = undefined

outputPlayersRemoved :: Maybe [Player] -> GameStateT ()
outputPlayersRemoved = undefined

outputHandValues :: GameStateT ()
outputHandValues = undefined

outputRoundNumber :: GameStateT ()
outputRoundNumber = undefined

outputSmallBlindMade :: GameStateT ()
outputSmallBlindMade = undefined

outputBigBlindMade :: GameStateT ()
outputBigBlindMade = undefined
