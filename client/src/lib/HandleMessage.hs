{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module HandleMessage
(
    handleMsg
)
where


import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Data.List (sort)
import Control.Concurrent.MVar (takeMVar)
import System.Log.Logger (infoM)

import Control.Lens 
    (Zoom, Zoomed, Lens', (^.), (^..), (.=), (-=), (+=), zoom, traversed, 
     filtered)

import ClientTypes (CGameStateT, CGame)
import CLenses (game, qmlState, actionMade)

import GUIUpdate 
    (updateBets, updateNames, updateInPlay, updateCards, updateButtons,
     updateVisible, updateCurrentPlayer, updatePot)

import Lenses 
    (player, action, cCurrentBet, cPlayers, cUUID, cBets,
     cChips, cBet, cInPlay, cSmallBlindSize, cBigBlindSize, playerTurn,
     allCards, playerCards, mapping, removed, infos, imsg, cCommunityCards,
     cCards, cIsMe, hand, person, cCurrentPlayer, cPot)

import Types 
    (Message(..), Action(..), ActionMsg, CPlayer, Card, PlayerHandInfo, CBets)

handleMsg :: Message -> CGameStateT (Maybe (Action Int))
handleMsg msg = do
    lift . infoM "Prog.handleMsg" $ "Recieved message: " ++ show msg

    case msg of
        MIsAction m -> def $ handleAction m
        MIsPlayerTurn m -> def $ handlePlayerTurn (m^.playerTurn)
        MIsCard m -> def $ handleNewCards (m^.allCards)
        MIsDealt m -> def $ handleMyCards (m^.playerCards)
        MIsNewChips m -> def $ handleNewChips (m^.mapping)
        MIsGameOver _ -> def handleGameOver
        MIsPlayersRemoved m -> def $ handlePlayersRemoved (m^.removed)
        MIsCardReveal m -> def $ handleCardsRevealed (m^.infos)
        MIsInput m -> handleInputRequest (m^.imsg)
        MIsBadInput _ -> def handleBadInput
        MIsInitialGame _ -> error "Unexpected initialGame in handleMsg!"
        MIsGatherChips _ -> def handleGatherChips
        MIsResetRound _ -> def handleResetRound
    where def f = f >> return Nothing

handleAction :: ActionMsg a -> CGameStateT ()
handleAction a = case a^.action of
    Fold -> fold (a^.player)
    Check -> return ()
    Call -> call (a^.player)
    Raise n -> raise n (a^.player)
    AllIn -> allIn (a^.player)
    SmallBlind -> smallBlind (a^.player)
    BigBlind -> bigBlind (a^.player)

updatePlayer :: (Applicative (Zoomed m1 a), Zoom m1 m CPlayer CGame) => 
                 UUID -> m1 a -> m b -> m b
updatePlayer u actions updates = do
    zoom (game.cPlayers.traversed.filtered isP) actions

    updates
    
    where isP p = p^.cUUID == u

fold :: UUID -> CGameStateT ()
fold u = updatePlayer u actions updates
    where actions = cInPlay .= False
          updates = updateInPlay

call :: UUID -> CGameStateT ()
call u = do
    s <- get

    let currBet = s^.game.cBets.cCurrentBet

    zoom (game.cPlayers.traversed.filtered isP) $ do
        p <- get
        cChips -= (currBet - p^.cBet)
        cBet .= currBet

    updateBets
    updateNames
    where isP p = p^.cUUID == u

raise :: Int -> UUID -> CGameStateT ()
raise n u = do
    zoom (game.cPlayers.traversed.filtered isP) $ do
        p <- get
        cChips -= (n - p^.cBet)
        cBet .= n

    game.cBets.cCurrentBet .= n

    updateBets
    updateNames

    where isP p = p^.cUUID == u

allIn :: UUID -> CGameStateT ()
allIn u = do
    zoom (game.cPlayers.traversed.filtered isP) $ do
        p <- get
        cBet .= (p^.cChips + p^.cBet)
        cChips .= 0

    s <- get

    game.cBets.cCurrentBet .= maximum (s^..game.cPlayers.traversed.cBet)

    updateBets
    updateNames

    where isP p = p^.cUUID == u

smallBlind :: UUID -> CGameStateT ()
smallBlind u = blind u cSmallBlindSize

bigBlind :: UUID -> CGameStateT ()
bigBlind u = blind u cBigBlindSize

blind :: UUID -> Lens' CBets Int -> CGameStateT ()
blind u lens = do
    s <- get

    let blindSize = s^.game.cBets.lens

    zoom (game.cPlayers.traversed.filtered isP) $ do
        cChips -= blindSize
        cBet .= blindSize

    game.cBets.cCurrentBet .= blindSize

    updateBets
    updateNames
    
    where isP p = p^.cUUID == u

handlePlayerTurn :: UUID -> CGameStateT ()
handlePlayerTurn u = do
    game.cCurrentPlayer .= u

    updateCurrentPlayer

handleNewCards :: [Card] -> CGameStateT ()
handleNewCards cs = do
    game.cCommunityCards .= cs
    updateCards

handleMyCards :: [Card] -> CGameStateT ()
handleMyCards cs = do
    zoom (game.cPlayers.traversed.filtered (^.cIsMe)) $
        cCards .= cs

    updateCards

handleNewChips :: [(UUID, Int)] -> CGameStateT ()
handleNewChips [] = do
    updateNames
    updateBets

handleNewChips ((u,n):xs) = do
    zoom (game.cPlayers.traversed.filtered isP) $ do
        cChips .= n        
        cBet .= 0

    handleNewChips xs

    where isP p = p^.cUUID == u

--need to show message box, then kill game once confirmed
--return to caller? in a different thread...
handleGameOver :: CGameStateT ()
handleGameOver = undefined

handlePlayersRemoved :: [UUID] -> CGameStateT ()
handlePlayersRemoved ids = do
    s <- get

    game.cPlayers .= filter isIn (s^.game.cPlayers)

    updateNames
    updateBets
    updateCards
    updateVisible

    where isIn x = x^.cUUID `notElem` ids

handleCardsRevealed :: [PlayerHandInfo] -> CGameStateT ()
handleCardsRevealed [] = updateCards
handleCardsRevealed (p:ps) = do
    zoom (game.cPlayers.traversed.filtered isP) $
        cCards .= p^.hand

    handleCardsRevealed ps

    where isP x = x^.cUUID == p^.person

--list of valid actions
handleInputRequest :: [Action Int] -> CGameStateT (Maybe (Action Int))
handleInputRequest as = do
    let boolList = buttonMap 0 . sort $ map actionButtonMap as
    
    updateButtons boolList
    
    s <- get
    
    -- this will block until the user clicks a button and the MVar gets updated
    action' <- lift $ takeMVar (s^.qmlState.actionMade)

    return $ Just action'

buttonMap :: Int -> [Int] -> [Bool]
buttonMap _ [] = []
buttonMap n full@(x:xs)
    | n >= 5 = []
    | n == x = True : buttonMap (n+1) xs
    | otherwise = False : buttonMap (n+1) full

actionButtonMap :: Action Int -> Int
actionButtonMap a = case a of
    Fold -> 0
    Check -> 1
    Call -> 2
    Raise _ -> 3
    AllIn -> 4
    _ -> error "Unexpected action in actionButtonMap!"

handleBadInput :: CGameStateT ()
handleBadInput = error $ 
    "Server reported it recieved invalid data! Ensure you have the latest " ++
    "version of both client and server"

handleGatherChips :: CGameStateT ()
handleGatherChips = do
    s <- get

    let bets = sum $ s^..game.cPlayers.traversed.cBet

    game.cPlayers.traversed.cBet .= 0

    game.cBets.cPot += bets

    updateBets
    updatePot

handleResetRound :: CGameStateT ()
handleResetRound = do
    game.cPlayers.traversed.cCards .= []
    game.cCommunityCards .= []
    game.cBets.cPot .= 0
    game.cPlayers.traversed.cBet .= 0

    updateCards
    updateBets
    updatePot
