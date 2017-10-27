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
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (takeMVar)
import System.Log.Logger (infoM)
import Control.Monad (when)
import Data.Text (Text)

import Control.Lens 
    (Zoom, Zoomed, Lens', (^.), (^..), (.=), (-=), (+=), zoom, traversed, 
     filtered)

import ClientTypes (CGameStateT, CGame)

import CLenses 
    (game, qmlState, actionMade, winWindowVisibleSig, winWindowVisibleS,
     lossWindowVisibleSig, lossWindowVisibleS)

import GUIUpdate 
    (updateBets, updateNames, updateInPlay, updateCards, updateButtons,
     updateVisible, updateCurrentPlayer, updatePot, updateRaiseWindow,
     showGameOverWindow, updateTextBox, createConsoleNewLine)

import Lenses 
    (player, action, cCurrentBet, cPlayers, cUUID, cBets, cChips, cBet, 
     cInPlay, cSmallBlindSize, cBigBlindSize, cCommunityCards, cCards, cIsMe, 
     hand, person, cCurrentPlayer, cPot, cMinimumRaise, cBigBlindSize, cUUID)

import Types 
    (Message(..), Action(..), ActionMsg, CPlayer, Card, PlayerHandInfo, CBets,
     PlayerTurnMsg(..), CardMsg(..), DealtCardsMsg(..), NewChipsMsg(..),
     PlayersRemovedMsg(..), CardRevealMsg(..), InputMsg(..), MinRaiseMsg(..),
     TextMsg(..))

handleMsg :: Message -> CGameStateT (Maybe (Action Int))
handleMsg msg = do
    lift . infoM "Prog.handleMsg" $ "Recieved message: " ++ show msg

    case msg of
        MIsAction m -> def $ handleAction m
        MIsPlayerTurn (PlayerTurnMsg m) -> def $ handlePlayerTurn m
        MIsCard (CardMsg m) -> def $ handleNewCards m
        MIsDealt (DealtCardsMsg m) -> def $ handleMyCards m
        MIsNewChips (NewChipsMsg m) -> def $ handleNewChips m
        MIsGameOver -> def handleGameOver
        MIsPlayersRemoved (PlayersRemovedMsg m) -> def $ handlePlayersRemoved m
        MIsCardReveal (CardRevealMsg m) -> def $ handleCardsRevealed m
        MIsInput (InputMsg m) -> handleInputRequest m
        MIsBadInput -> def handleBadInput
        MIsInitialGame _ -> error "Unexpected initialGame in handleMsg!"
        MIsGatherChips -> def handleGatherChips
        MIsResetRound -> def handleResetRound
        MIsNextState -> def handleNextState
        MIsMinRaise (MinRaiseMsg m) -> def $ handleMinRaise m
        MIsTextMsg (TextMsg m) -> def $ handleTextMsg m
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
    game.cBets.cPot .= 0

    updateNames
    updateBets
    updatePot

handleNewChips ((u,n):xs) = do
    zoom (game.cPlayers.traversed.filtered isP) $ do
        cChips .= n        
        cBet .= 0

    handleNewChips xs

    where isP p = p^.cUUID == u

handleGameOver :: CGameStateT ()
handleGameOver = showGameOverWindow winWindowVisibleSig winWindowVisibleS

handleOutOfChips :: CGameStateT ()
handleOutOfChips = showGameOverWindow lossWindowVisibleSig lossWindowVisibleS

handlePlayersRemoved :: [UUID] -> CGameStateT ()
handlePlayersRemoved ids = do
    s <- get

    let me = filter (^.cIsMe) (s^.game.cPlayers)

    if head me^.cUUID `elem` ids
        then do
            game.cPlayers .= me
            updateVisible
            handleOutOfChips
        else do
            game.cPlayers .= filter isIn (s^.game.cPlayers)
            updateVisible

    where isIn x = x^.cUUID `notElem` ids

handleCardsRevealed :: [PlayerHandInfo] -> CGameStateT ()
handleCardsRevealed [] = do
    updateCards
    lift $ threadDelay (5 * oneSecond)

    where oneSecond = 10^6

handleCardsRevealed (p:ps) = do
    zoom (game.cPlayers.traversed.filtered isP) $
        cCards .= p^.hand

    handleCardsRevealed ps

    where isP x = x^.cUUID == p^.person

--list of valid actions
handleInputRequest :: [Action Int] -> CGameStateT (Maybe (Action Int))
handleInputRequest as = do
    let actions = map actionButtonMap as
        boolList = buttonMap 0 $ sort actions
    
    when (raiseVal `elem` actions) updateRaiseWindow
    
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
    Fold -> foldVal
    Check -> checkVal
    Call -> callVal
    Raise _ -> raiseVal
    AllIn -> allInVal
    _ -> error "Unexpected action in actionButtonMap!"

foldVal :: Int
foldVal = 0

checkVal :: Int
checkVal = 1

callVal :: Int
callVal = 2

raiseVal :: Int
raiseVal = 3

allInVal :: Int
allInVal = 4

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
    s <- get

    game.cPlayers.traversed.cCards .= []
    game.cPlayers.traversed.cInPlay .= True
    game.cCommunityCards .= []
    game.cBets.cPot .= 0
    game.cBets.cMinimumRaise .= (s^.game.cBets.cBigBlindSize)
    game.cPlayers.traversed.cBet .= 0

    updateCards
    updateBets
    updatePot
    updateInPlay
    createConsoleNewLine

handleMinRaise :: Int -> CGameStateT ()
handleMinRaise newRaise = game.cBets.cMinimumRaise .= newRaise

handleNextState :: CGameStateT ()
handleNextState = do
    s <- get

    game.cBets.cMinimumRaise .= s^.game.cBets.cBigBlindSize
    game.cBets.cCurrentBet .= 0

handleTextMsg :: [Text] -> CGameStateT ()
handleTextMsg = updateTextBox
