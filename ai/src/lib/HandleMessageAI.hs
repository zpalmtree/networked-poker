{-# LANGUAGE RankNTypes #-}

module HandleMessageAI
(
    handleAction,
    handleNewCards,
    handleMyCards,
    handleNewChips,
    handlePlayersRemoved,
    handleBadInput,
    handleGatherChips,
    handleResetRound,
    handleNextState,
    handleMinRaise,
    handleGameOver
)
where

import Data.UUID.Types (UUID)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import System.Exit (exitSuccess)

import Control.Lens 
    (Lens', (^.), (^..), (.=), (-=), (+=), zoom, traversed, filtered)

import AITypes (AIGameStateT)
import ClientFramework (ClientResponse(..))
import Types (ActionMsg, Action(..), CBets, Card)

import Lenses 
    (player, action, cCurrentBet, cPlayers, cUUID, cBets, cChips, cBet, 
     cInPlay, cSmallBlindSize, cBigBlindSize, cCommunityCards, cCards, cIsMe, 
     cPot, cMinimumRaise, cBigBlindSize, cUUID)

handleAction :: ActionMsg a -> AIGameStateT ()
handleAction a = case a^.action of
    Fold -> fold (a^.player)
    Check -> return ()
    Call -> call (a^.player)
    Raise n -> raise n (a^.player)
    AllIn -> allIn (a^.player)
    SmallBlind -> smallBlind (a^.player)
    BigBlind -> bigBlind (a^.player)

fold :: UUID -> AIGameStateT ()
fold u = cPlayers.traversed.filtered isP.cInPlay .= False
    where isP p = p^.cUUID == u

call :: UUID -> AIGameStateT ()
call u = do
    s <- get

    let currBet = s^.cBets.cCurrentBet

    zoom (cPlayers.traversed.filtered isP) $ do
        p <- get
        cChips -= (currBet - p^.cBet)
        cBet .= currBet

    where isP p = p^.cUUID == u

raise :: Int -> UUID -> AIGameStateT ()
raise n u = do
    zoom (cPlayers.traversed.filtered isP) $ do
        p <- get
        cChips -= (n - p^.cBet)
        cBet .= n

    cBets.cCurrentBet .= n

    where isP p = p^.cUUID == u

allIn :: UUID -> AIGameStateT ()
allIn u = do
    zoom (cPlayers.traversed.filtered isP) $ do
        p <- get
        cBet .= (p^.cChips + p^.cBet)
        cChips .= 0

    s <- get

    cBets.cCurrentBet .= maximum (s^..cPlayers.traversed.cBet)

    where isP p = p^.cUUID == u

smallBlind :: UUID -> AIGameStateT ()
smallBlind u = blind u cSmallBlindSize

bigBlind :: UUID -> AIGameStateT ()
bigBlind u = blind u cBigBlindSize

blind :: UUID -> Lens' CBets Int -> AIGameStateT ()
blind u lens = do
    s <- get

    let blindSize = s^.cBets.lens

    zoom (cPlayers.traversed.filtered isP) $ do
        cChips -= blindSize
        cBet .= blindSize

    cBets.cCurrentBet .= blindSize

    where isP p = p^.cUUID == u

handleNewCards :: [Card] -> AIGameStateT ()
handleNewCards cs = cCommunityCards .= cs

handleMyCards :: [Card] -> AIGameStateT ()
handleMyCards cs = cPlayers.traversed.filtered (^.cIsMe).cCards .= cs

handleNewChips :: [(UUID, Int)] -> AIGameStateT ()
handleNewChips [] = cBets.cPot .= 0

handleNewChips ((u,n):xs) = do
    zoom (cPlayers.traversed.filtered isP) $ do
        cChips .= n        
        cBet .= 0

    handleNewChips xs

    where isP p = p^.cUUID == u

handlePlayersRemoved :: [UUID] -> AIGameStateT (Maybe Bool)
handlePlayersRemoved ids = do
    s <- get

    let me = filter (^.cIsMe) (s^.cPlayers)

    if head me^.cUUID `elem` ids
        then return $ Just True

        else do
            cPlayers .= filter isIn (s^.cPlayers)
            return Nothing

    where isIn x = x^.cUUID `notElem` ids

handleBadInput :: AIGameStateT ()
handleBadInput = error $ 
    "Server reported it recieved invalid data! Ensure you have the latest " ++
    "version of both client and server"

handleGatherChips :: AIGameStateT ()
handleGatherChips = do
    s <- get

    let bets = sum $ s^..cPlayers.traversed.cBet

    cPlayers.traversed.cBet .= 0

    cBets.cPot += bets

handleResetRound :: AIGameStateT ()
handleResetRound = do
    s <- get

    cPlayers.traversed.cCards .= []
    cPlayers.traversed.cInPlay .= True
    cCommunityCards .= []
    cBets.cPot .= 0
    cBets.cMinimumRaise .= (s^.cBets.cBigBlindSize)
    cPlayers.traversed.cBet .= 0

handleMinRaise :: Int -> AIGameStateT ()
handleMinRaise newRaise = cBets.cMinimumRaise .= newRaise

handleNextState :: AIGameStateT ()
handleNextState = do
    s <- get

    cBets.cMinimumRaise .= s^.cBets.cBigBlindSize
    cBets.cCurrentBet .= 0

handleGameOver :: AIGameStateT (ClientResponse a)
handleGameOver = return GameWon
