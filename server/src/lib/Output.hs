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
    outputInitialGame,
    outputInputRequest,
    outputBadInput,
    outputGatherChips,
    outputResetRound,
    outputUpdateMinRaise,
    outputNextState
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
import System.Log.Logger (infoM)
import Text.Printf (printf)

import Utilities.Player (getCurrentPlayer, getCurrentPlayerUUID)
import HumanMessage (humanHandValues, humanAction, humanNewChips)

import Lenses
    (socket, uuid, playerQueue, players, cardInfo, tableCards, cards, inPlay,
     handInfo, handValue, cUUID, cPlayers, cCards, cIsMe, chips)

import Types 
    (Message(..), PlayerTurnMsg(..), ActionMsg(..), CardMsg(..),
     DealtCardsMsg(..), NewChipsMsg(..), PlayersRemovedMsg(..), 
     CardRevealMsg(..), PlayerHandInfo(..), InitialGameMsg(..), GameStateT, 
     Action, Player, ClientGame, InputMsg(..), MinRaiseMsg(..), TextMsg(..))

msgAll :: Message -> GameStateT ()
msgAll msg = do
    s <- get

    mapM_ (msgP msg) (s^.playerQueue.players)

msgP :: Message -> Player -> GameStateT ()
msgP msg p = do
    let sendMsg sock = lift $ send sock (toStrict $ encode msg)

    void . sendMsg $ p^.socket

    lift . infoM "Prog.msgP" $ printf "Sending message (%s) to %s\n" (show msg) 
                                      (show $ p^.uuid)

outputBadInput :: GameStateT ()
outputBadInput = do
    p <- getCurrentPlayer

    msgP MIsBadInput p

outputInputRequest :: [Action Int] -> GameStateT ()
outputInputRequest actions = do
    p <- getCurrentPlayer

    let msg = MIsInput $ InputMsg actions

    msgP msg p

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

    humanMsg' <- humanAction a

    let msg = MIsAction $ ActionMsg a (p^.uuid)
        humanMsg = MIsTextMsg $ TextMsg humanMsg'

    msgAll msg
    msgAll humanMsg

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

outputNewChips :: [(Int, [Player])] -> GameStateT ()
outputNewChips pot2player = do
    s <- get

    let mapping = zip (s^..playerQueue.players.traversed.uuid)
                      (s^..playerQueue.players.traversed.chips)

        humanMsg = humanNewChips pot2player

    msgAll . MIsTextMsg $ TextMsg humanMsg
    msgAll . MIsNewChips $ NewChipsMsg mapping

outputGameOver :: GameStateT ()
outputGameOver = msgAll MIsGameOver

outputPlayersRemoved :: Maybe [UUID] -> GameStateT ()
outputPlayersRemoved maybeP = case maybeP of
    Nothing -> return ()
    Just p -> msgAll . MIsPlayersRemoved $ PlayersRemovedMsg p

outputHandValues :: GameStateT ()
outputHandValues = do
    s <- get

    let inPlayers = filter (^.inPlay) (s^.playerQueue.players)
        details = map mkHandInfo inPlayers
        humanMsg = humanHandValues inPlayers

    msgAll . MIsTextMsg $ TextMsg humanMsg
    msgAll . MIsCardReveal $ CardRevealMsg details

    where mkHandInfo p = PlayerHandInfo (p^.uuid) 
                                        (fromJust (p^.handInfo)^.handValue)
                                        (p^.cards)

outputGatherChips :: GameStateT ()
outputGatherChips = msgAll MIsGatherChips

outputResetRound :: GameStateT ()
outputResetRound = msgAll MIsResetRound

outputUpdateMinRaise :: Int -> GameStateT ()
outputUpdateMinRaise n = msgAll . MIsMinRaise $ MinRaiseMsg n

outputNextState :: GameStateT ()
outputNextState = msgAll MIsNextState
