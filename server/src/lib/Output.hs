module Output
(
    outputAction,
    outputPlayerTurn,
    outputCards,
    outputPlayerCards,
    outputNewChips,
    outputGameOver,
    outputPlayersRemoved,
    outputHandValues,
    outputInitialGame
)
where

import Data.UUID.Types (UUID)
import Network.Socket.ByteString (send)
import Control.Lens ((^.), (^..), (.=), (%=), zoom, filtered, traversed)
import Data.Binary (encode)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Trans.State (StateT, get, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (void, forM_)
import Data.Maybe (fromJust)

import Utilities.Player (getCurrentPlayer, getCurrentPlayerUUID)

import Lenses
    (socket, uuid, playerQueue, players, cardInfo, tableCards, cards, inPlay,
     handInfo, handValue, cUUID, cPlayers, cCards, cIsMe, chips)

import Types 
    (Message(..), PlayerTurnMsg(..), ActionMsg(..), CardMsg(..),
     DealtCardsMsg(..), NewChipsMsg(..), GameOverMsg(..),
     PlayersRemovedMsg(..), CardRevealMsg(..), PlayerHandInfo(..),
     InitialGameMsg(..), GameStateT, Action, Player, ClientGame)

msgAll :: Message -> GameStateT ()
msgAll msg = do
    s <- get

    mapM_ (msgP msg) (s^.playerQueue.players)

msgP :: Message -> Player -> GameStateT ()
msgP msg p = do
    let sendMsg sock = lift $ send sock (toStrict $ encode msg)

    void . sendMsg $ p^.socket

outputInitialGame :: ClientGame -> GameStateT ()
outputInitialGame cgame = do
    s <- get

    forM_ (s^.playerQueue.players) $ \p -> do
        newState <- lift $ execStateT (mkPersonalMsg p) cgame
        let msg = MIsInitialGame $ InitialGameMsg newState
        msgP msg p

mkPersonalMsg :: Monad m => Player -> StateT ClientGame m ()
mkPersonalMsg p = do
    s <- get

    zoom (cPlayers.traversed.filtered isMe) $ do
        cCards .= (p^.cards)
        cIsMe .= True

    let pos = myPos (s^.cPlayers)

    cPlayers %= shift pos

    where shift n ps = let (a, b) = splitAt n ps
                       in b ++ a

          isMe cp = cp^.cUUID == p^.uuid

          myPos [] = error "Couldn't find in myPos!"
          myPos (x:xs)
            | isMe x = 0
            | otherwise = 1 + myPos xs
        
outputAction :: Action Int -> GameStateT ()
outputAction a = do
    p <- getCurrentPlayer

    let msg = MIsAction $ ActionMsg a (p^.uuid)

    msgAll msg

outputPlayerTurn :: GameStateT ()
outputPlayerTurn = do
    u <- getCurrentPlayerUUID

    let msg = MIsPlayerTurn $ PlayerTurnMsg u

    msgAll msg

outputCards :: GameStateT ()
outputCards = do
    s <- get

    let msg = MIsCard $ CardMsg (s^.cardInfo.tableCards)

    msgAll msg

outputPlayerCards :: GameStateT ()
outputPlayerCards = do
    s <- get

    mapM_ (\p -> msgP (msg p) p) (s^.playerQueue.players)

    where msg p = MIsDealt $ DealtCardsMsg (p^.cards)

outputNewChips :: GameStateT ()
outputNewChips = do
    s <- get

    let mapping = zip (s^..playerQueue.players.traversed.uuid)
                      (s^..playerQueue.players.traversed.chips)

    msgAll . MIsNewChips $ NewChipsMsg mapping

outputGameOver :: GameStateT ()
outputGameOver = msgAll . MIsGameOver $ GameOverMsg

outputPlayersRemoved :: Maybe [UUID] -> GameStateT ()
outputPlayersRemoved maybeP = case maybeP of
    Nothing -> return ()
    Just p -> msgAll . MIsPlayersRemoved $ PlayersRemovedMsg p

outputHandValues :: GameStateT ()
outputHandValues = do
    s <- get

    let inPlayers = filter (^.inPlay) (s^.playerQueue.players)
        details = map mkHandInfo inPlayers

    msgAll . MIsCardReveal $ CardRevealMsg details

    where mkHandInfo p = PlayerHandInfo (p^.uuid) 
                                        (fromJust (p^.handInfo)^.handValue)
                                        (p^.cards)
