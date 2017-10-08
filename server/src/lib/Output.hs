module Output
(
    outputAction,
    outputPlayerTurn,
    outputCards,
    outputPlayerCards,
    outputWinners,
    outputGameOver,
    outputPlayersRemoved,
    outputHandValues
)
where

import Data.UUID.Types (UUID)
import Network.Socket.ByteString (send)
import Control.Lens ((^.))
import Data.Binary (Binary, encode)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (void)
import Data.Maybe (fromJust)

import Utilities.Player (getCurrentPlayerT, getCurrentPlayerUUID)

import Lenses
    (socket, uuid, playerQueue, players, cardInfo, tableCards, cards, inPlay,
     handInfo, handValue)

import Types 
    (Message(..), PlayerTurnMessage(..), ActionMessage(..), CardMessage(..),
     DealtCardsMessage(..), PotWinnersMessage(..), GameOverMessage(..),
     PlayersRemovedMessage(..), CardRevealMessage(..), PlayerHandInfo(..),
     GameStateT, Action, Player, Pot)

msgAll :: (Binary a) => Message a -> GameStateT ()
msgAll msg = do
    s <- get

    mapM_ (msgP msg) (s^.playerQueue.players)

msgP :: (Binary a) => Message a -> Player -> GameStateT ()
msgP msg p = do
    let sendMsg sock = lift $ send sock (toStrict $ encode msg)

    void . sendMsg $ p^.socket

outputAction :: Action Int -> GameStateT ()
outputAction a = do
    p <- getCurrentPlayerT

    let msg = Message $ ActionMessage a (p^.uuid)

    msgAll msg

outputPlayerTurn :: GameStateT ()
outputPlayerTurn = do
    u <- getCurrentPlayerUUID

    let msg = Message $ PlayerTurnMessage u

    msgAll msg

outputCards :: GameStateT ()
outputCards = do
    s <- get

    let msg = Message $ CardMessage (s^.cardInfo.tableCards)

    msgAll msg

outputPlayerCards :: GameStateT ()
outputPlayerCards = do
    s <- get

    mapM_ (\p -> msgP (msg p) p) (s^.playerQueue.players)

    where msg p = Message $ DealtCardsMessage (p^.cards)

outputWinners :: [(Pot, [UUID])] -> GameStateT ()
outputWinners potWinnerMap = msgAll . Message $ PotWinnersMessage potWinnerMap

outputGameOver :: GameStateT ()
outputGameOver = msgAll . Message $ GameOverMessage

outputPlayersRemoved :: Maybe [UUID] -> GameStateT ()
outputPlayersRemoved maybeP = case maybeP of
    Nothing -> return ()
    Just p -> msgAll . Message $ PlayersRemovedMessage p

outputHandValues :: GameStateT ()
outputHandValues = do
    s <- get

    let inPlayers = filter (^.inPlay) (s^.playerQueue.players)
        details = map mkHandInfo inPlayers

    msgAll . Message $ CardRevealMessage details

    where mkHandInfo p = PlayerHandInfo (p^.uuid) 
                                        (fromJust (p^.handInfo)^.handValue)
                                        (p^.cards)
