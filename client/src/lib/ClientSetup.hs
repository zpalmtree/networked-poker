module ClientSetup
(
    initialSetup,
    initialGUISetup,
    makeClass
)
where

import Control.Concurrent.MVar (newEmptyMVar)
import Data.IORef (newIORef, readIORef)
import Data.Text (pack)
import Network.Socket (Socket(..))

import Graphics.QML 
    (Class, ObjRef, defPropertySigRO', newClass, newSignalKey, 
     defMethod')

import ClientFramework (establishConnection)
import ClientTypes (StatesNSignals(..), CGameStateT, CGame(..))
import Constants (maxPlayers, numTCards, numButtons, cardBack)
import HandleClick (handleFold, handleCheck, handleCall, handleRaise, handleAllIn)

import GUIUpdate 
    (updateNames, updateBets, updateCards, updateVisible, updateCurrentPlayer)

initialSetup :: StatesNSignals -> ObjRef () -> String -> 
                IO (Either String (CGame, Socket))
initialSetup sigs this name = do
    maybeGame <- establishConnection name
    case maybeGame of
        Right (game, sock) -> return $ Right (CGame game sigs this, sock)
        Left err -> return $ Left err

makeClass :: IO (Class (), StatesNSignals)
makeClass = do
    let cards = replicate 2 cardBack

    -- BUTTONS
    bEnabledSig <- newSignalKey
    bEnabledS <- newIORef $ replicate numButtons False

    -- TABLE CARDS
    tCardsSig <- newSignalKey
    tCardsS <- newIORef $ replicate numTCards cardBack

    -- PLAYER CARDS
    pCardsSig <- newSignalKey
    pCardsS <- newIORef $ replicate maxPlayers cards

    -- PLAYER CHIPS
    pChipsSig <- newSignalKey
    pChipsS <- newIORef $ replicate maxPlayers 0

    -- POT CHIPS
    potChipsSig <- newSignalKey
    potChipsS <- newIORef 0

    -- PLAYER NAMES
    pNamesSig <- newSignalKey
    pNamesS <- newIORef names

    -- PLAYERS VISIBLE
    pVisibleSig <- newSignalKey
    pVisibleS <- newIORef $ replicate maxPlayers True

    -- PLAYERS INPLAY
    pInPlaySig <- newSignalKey
    pInPlayS <- newIORef $ replicate maxPlayers True

    --CURRENT PLAYER BORDER
    pCurrentPlayerSig <- newSignalKey
    pCurrentPlayerS <- newIORef $ replicate maxPlayers False

    --SLIDER VALUES
    slideMinSig <- newSignalKey
    slideMaxSig <- newSignalKey
    slideMinS <- newIORef 0
    slideMaxS <- newIORef 0

    --GAME OVER WINDOWS
    lossWindowVisibleSig <- newSignalKey
    winWindowVisibleSig <- newSignalKey
    lossWindowVisibleS <- newIORef False
    winWindowVisibleS <- newIORef False

    --LOG MESSAGES
    logMsgSig <- newSignalKey
    logMsgS <- newIORef []

    m <- newEmptyMVar

    let sNs = StatesNSignals pCardsSig              pCardsS
                             pChipsSig              pChipsS
                             pNamesSig              pNamesS
                             tCardsSig              tCardsS
                             bEnabledSig            bEnabledS
                             potChipsSig            potChipsS
                             pVisibleSig            pVisibleS
                             pInPlaySig             pInPlayS
                             pCurrentPlayerSig      pCurrentPlayerS
                             slideMinSig            slideMinS
                             slideMaxSig            slideMaxS
                             lossWindowVisibleSig   lossWindowVisibleS
                             winWindowVisibleSig    winWindowVisibleS
                             logMsgSig              logMsgS
                             m

    rootClass <- newClass [
        defPropertySigRO' "bEnabled" bEnabledSig $ defRead bEnabledS,
        defPropertySigRO' "pVisible" pVisibleSig $ defRead pVisibleS,
        defPropertySigRO' "pInPlay" pInPlaySig $ defRead pInPlayS,
        defPropertySigRO' "pCurrentPlayer" pCurrentPlayerSig 
                        $ defRead pCurrentPlayerS,
        defPropertySigRO' "tCards" tCardsSig $ defRead tCardsS,
        defPropertySigRO' "pNames" pNamesSig $ defRead pNamesS,
        defPropertySigRO' "pCards" pCardsSig $ defRead pCardsS,
        defPropertySigRO' "potValue" potChipsSig $ defRead potChipsS,
        defPropertySigRO' "pBets" pChipsSig $ defRead pChipsS,
        defPropertySigRO' "slideMin" slideMinSig $ defRead slideMinS,
        defPropertySigRO' "slideMax" slideMaxSig $ defRead slideMaxS,
        defPropertySigRO' "lossWindowVisible" lossWindowVisibleSig 
                        $ defRead lossWindowVisibleS,
        defPropertySigRO' "winWindowVisible" winWindowVisibleSig
                        $ defRead winWindowVisibleS,
        defPropertySigRO' "messages" logMsgSig $ defRead logMsgS,

        defMethod' "fold" (handleFold sNs),
        defMethod' "check" (handleCheck sNs),
        defMethod' "call" (handleCall sNs),
        defMethod' "raiseN" (handleRaise sNs),
        defMethod' "allIn" (handleAllIn sNs)]

    return (rootClass, sNs)

    where defRead s _ = readIORef s
          names = replicate maxPlayers (pack "")

initialGUISetup :: CGameStateT ()
initialGUISetup = do
    updateVisible
    updateNames
    updateBets
    updateCards
    updateCurrentPlayer
