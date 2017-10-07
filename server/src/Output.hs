module Output
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
    outputSmallBlindMade,
    outputBigBlindMade
)
where

import Data.UUID.Types (UUID)
import Network.Socket.ByteString (send)
import Control.Lens ((^.))
import Data.Binary (encode)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Trans.Class (lift)
import Control.Monad (void)

import Utilities.Player (getCurrentPlayerT)
import Types (GameStateT, Action, Player, Pot, ActionMessage(..))
import Lenses (socket, uuid)

outputAction :: Action Int -> GameStateT ()
outputAction a = do
    p <- getCurrentPlayerT
    void . lift $ 
        send (p^.socket) (toStrict . encode $ ActionMessage a (p^.uuid))

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

outputSmallBlindMade :: GameStateT ()
outputSmallBlindMade = undefined

outputBigBlindMade :: GameStateT ()
outputBigBlindMade = undefined
