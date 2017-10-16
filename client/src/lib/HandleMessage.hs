{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module HandleMessage
(
    handleMsg
)
where


import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (get)
import Data.List (sort)

import Control.Lens 
    (Zoom, Zoomed, Lens', (^.), (.=), (-=), zoom, traversed, filtered)

import ClientTypes (CGameStateT, CGame)
import CLenses (game)

import GUIUpdate 
    (updateBets, updateNames, updateInPlay, updateCards, updateButtons,
     updateVisible)

import Lenses 
    (player, action, currentBet, cPlayerQueue, cPlayers, cUUID, cBets,
     cChips, cBet, cInPlay, smallBlindSize, bigBlindSize, playerTurn,
     allCards, playerCards, mapping, removed, infos, imsg, cCommunityCards,
     cCards, cIsMe, hand, person)

import Types 
    (Message(..), Action(..), ActionMsg, CPlayer, Bets, Card, PlayerHandInfo)

handleMsg :: Message -> CGameStateT ()
handleMsg msg = case msg of
    MIsAction m -> handleAction m
    MIsPlayerTurn m -> handlePlayerTurn (m^.playerTurn)
    MIsCard m -> handleNewCards (m^.allCards)
    MIsDealt m -> handleMyCards (m^.playerCards)
    MIsNewChips m -> handleNewChips (m^.mapping)
    MIsGameOver _ -> handleGameOver
    MIsPlayersRemoved m -> handlePlayersRemoved (m^.removed)
    MIsCardReveal m -> handleCardsRevealed (m^.infos)
    MIsInput m -> handleInputRequest (m^.imsg)
    MIsBadInput _ -> handleBadInput
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

updatePlayer :: (Applicative (Zoomed m1 a), Zoom m1 m CPlayer CGame) => 
                 UUID -> m1 a -> m b -> m b
updatePlayer u actions updates = do
    zoom (game.cPlayerQueue.cPlayers.traversed.filtered isP) actions

    updates
    
    where isP p = p^.cUUID == u

fold :: UUID -> CGameStateT ()
fold u = updatePlayer u actions updates
    where actions = cInPlay .= False
          updates = updateInPlay

call :: UUID -> CGameStateT ()
call u = do
    s <- get

    let currBet = s^.game.cBets.currentBet

    zoom (game.cPlayerQueue.cPlayers.traversed.filtered isP) $ do
        p <- get
        cChips -= (currBet - p^.cBet)
        cBet .= currBet

    updateBets
    updateNames
    where isP p = p^.cUUID == u

raise :: Int -> UUID -> CGameStateT ()
raise n u = updatePlayer u actions updates
    where actions = do
            p <- get
            cChips -= (n - p^.cBet)
            cBet .= n

          updates = do
            updateBets
            updateNames

allIn :: UUID -> CGameStateT ()
allIn u = updatePlayer u actions updates
    where actions = do
            p <- get
            cBet .= (p^.cChips + p^.cBet)
            cChips .= 0

          updates = do
            updateBets
            updateNames

smallBlind :: UUID -> CGameStateT ()
smallBlind u = blind u smallBlindSize

bigBlind :: UUID -> CGameStateT ()
bigBlind u = blind u bigBlindSize

blind :: UUID -> Lens' Bets Int -> CGameStateT ()
blind u lens = do
    s <- get

    let blindSize = s^.game.cBets.lens

    zoom (game.cPlayerQueue.cPlayers.traversed.filtered isP) $ do
        cChips -= blindSize
        cBet .= blindSize

    updateBets
    updateNames
    
    where isP p = p^.cUUID == u

--need to make gui hint current player, maybe yellow outline or something
handlePlayerTurn :: UUID -> CGameStateT ()
handlePlayerTurn = undefined

handleNewCards :: [Card] -> CGameStateT ()
handleNewCards cs = do
    game.cCommunityCards .= cs
    updateCards

handleMyCards :: [Card] -> CGameStateT ()
handleMyCards cs = do
    zoom (game.cPlayerQueue.cPlayers.traversed.filtered (^.cIsMe)) $ do
        cCards .= cs

    updateCards

handleNewChips :: [(UUID, Int)] -> CGameStateT ()
handleNewChips = undefined

--need to show message box, then kill game once confirmed
--return to caller? in a different thread...
handleGameOver :: CGameStateT ()
handleGameOver = undefined

handlePlayersRemoved :: [UUID] -> CGameStateT ()
handlePlayersRemoved ids = do
    s <- get

    game.cPlayerQueue.cPlayers .= filter isIn (s^.game.cPlayerQueue.cPlayers)

    updateNames
    updateBets
    updateCards
    updateVisible

    where isIn x = x^.cUUID `notElem` ids

handleCardsRevealed :: [PlayerHandInfo] -> CGameStateT ()
handleCardsRevealed [] = updateCards
handleCardsRevealed (p:ps) = do
    zoom (game.cPlayerQueue.cPlayers.traversed.filtered isP) $ do
        cCards .= p^.hand

    handleCardsRevealed ps

    where isP x = x^.cUUID == p^.person

--list of valid actions
handleInputRequest :: [Action Int] -> CGameStateT ()
handleInputRequest as = do
    let boolList = buttonMap 0 $ sort $ map actionButtonMap as
    
    updateButtons boolList

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
