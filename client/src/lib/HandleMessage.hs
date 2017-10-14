module HandleMessage
(
    handleMsg
)
where

import ClientTypes (CGameStateT)

import Types 
    (Message(..), ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, 
     PotWinnersMsg, GameOverMsg, PlayersRemovedMsg, CardRevealMsg, InputMsg,
     BadInputMsg)

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
handleAction = undefined

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
