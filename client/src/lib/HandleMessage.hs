module HandleMessage
(
    handleMsg
)
where

import Control.Lens ((^.), (.=), (-=), zoom, traversed, filtered)
import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (get)

import ClientTypes (CGameStateT)
import CLenses (game)
import GUIUpdate (updateBets, updateNames)

import Lenses 
    (player, action, currentBet, cPlayerQueue, cPlayers, cUUID, cBets,
     cChips, cBet)

import Types 
    (Message(..), ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, 
     PotWinnersMsg, GameOverMsg, PlayersRemovedMsg, CardRevealMsg, InputMsg,
     BadInputMsg, Action(..))

handleMsg :: Message -> CGameStateT ()
handleMsg msg = case msg of
    MIsAction m -> handleAction m
    MIsPlayerTurn m -> handlePlayerTurn m
    MIsCard m -> handleNewCards m
    MIsDealt m -> handleMyCards m
    MIsPotWinners m -> handlePotWinners m
    MIsGameOver m -> handleGameOver m
    MIsPlayersRemoved m -> handlePlayersRemoved m
    MIsCardReveal m -> handleCardsRevealed m
    MIsInput m -> handleInputRequest m
    MIsBadInput m -> handleBadInput m
    MIsInitialGame _ -> error "Unexpected initialGame in handleMsg!"

handleAction :: ActionMsg a -> CGameStateT ()
handleAction a = case a^.action of
    Fold -> fold (a^.player)
    Check -> return ()
    Call -> call (a^.player)
    Raise n -> raise n (a^.player)
    AllIn -> allIn (a^.player)
    SmallBlind -> smallBlind (a^.player)
    BigBlind -> bigBlind (a^.player)

call :: UUID -> CGameStateT ()
call u = do
    s <- get

    let currBet = s^.game.cBets.currentBet

    zoom (game.cPlayerQueue.cPlayers.traversed.filtered (\p -> p^.cUUID == u)) $ do
        p <- get
        cChips -= (currBet - p^.cBet)
        cBet .= currBet

    updateBets
    updateNames

fold = undefined
raise = undefined
allIn = undefined
smallBlind = undefined
bigBlind = undefined

handlePlayerTurn :: PlayerTurnMsg -> CGameStateT ()
handlePlayerTurn = undefined

handleNewCards :: CardMsg -> CGameStateT ()
handleNewCards = undefined

handleMyCards :: DealtCardsMsg -> CGameStateT ()
handleMyCards = undefined

handlePotWinners :: PotWinnersMsg -> CGameStateT ()
handlePotWinners = undefined

handleGameOver :: GameOverMsg -> CGameStateT ()
handleGameOver = undefined

handlePlayersRemoved :: PlayersRemovedMsg -> CGameStateT ()
handlePlayersRemoved = undefined

handleCardsRevealed :: CardRevealMsg -> CGameStateT ()
handleCardsRevealed = undefined

handleInputRequest :: InputMsg -> CGameStateT ()
handleInputRequest = undefined

handleBadInput :: BadInputMsg -> CGameStateT ()
handleBadInput = undefined
