{-# LANGUAGE RankNTypes #-}

module GUIUpdate
(
    updateNames,
    updateBets,
    updateCards,
    updateVisible,
    updateInPlay,
    updateButtons,
    updateCurrentPlayer,
    updatePot,
    updateRaiseWindow,
    showGameOverWindow,
    updateTextBox,
    createConsoleNewLine
)
where

import Data.Text (Text, pack, empty)
import Control.Lens (Lens', (^.), (^..), traversed)
import Graphics.QML (SignalKey, fireSignal)
import Data.IORef (IORef, writeIORef, atomicModifyIORef)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import ClientUtilities (cardToFileNameText)
import Constants (maxPlayers, cardBack, numTCards)
import ClientTypes (CGameStateT, StatesNSignals)
import Types (Card)

import Lenses 
    (cPlayers, cName, cChips, cCards, cBet, cInPlay, cCommunityCards,
     cCurrentPlayer, cUUID, cBets, cPot, cMinimumRaise, cIsMe, cCurrentBet)

import CLenses 
    (game, qmlState, pVisibleS, pVisibleSig, ctx, pNamesS, pNamesSig,
     pBetsS, pBetsSig, pCardsS, pCardsSig, pInPlayS, pInPlaySig, tCardsSig,
     tCardsS, bEnabledSig, bEnabledS, pCurrentPlayerSig, pCurrentPlayerS,
     potChipsS, potChipsSig, slideMinS, slideMaxS, slideMinSig, slideMaxSig,
     logMsgSig, logMsgS)

updateInPlay :: CGameStateT ()
updateInPlay = do
    s <- get

    let inPlay = s^..game.cPlayers.traversed.cInPlay
        padded = padMaxP inPlay False

    lift $ writeIORef (s^.qmlState.pInPlayS) padded
    lift $ fireSignal (s^.qmlState.pInPlaySig) (s^.ctx)

updateNames :: CGameStateT ()
updateNames = do
    s <- get

    let names = s^..game.cPlayers.traversed.cName
        chips = s^..game.cPlayers.traversed.cChips
        fullText = zipWith (\a b -> a ++ "\n" ++ show b) names chips
        padded = map pack $ padMaxP fullText ""

    lift $ writeIORef (s^.qmlState.pNamesS) padded
    lift $ fireSignal (s^.qmlState.pNamesSig) (s^.ctx)

updateBets :: CGameStateT ()
updateBets = do
    s <- get

    let bets = s^..game.cPlayers.traversed.cBet
        padded = padMaxP bets 0

    lift $ writeIORef (s^.qmlState.pBetsS) padded
    lift $ fireSignal (s^.qmlState.pBetsSig) (s^.ctx)

updatePot :: CGameStateT ()
updatePot = do
    s <- get

    let pot = s^.game.cBets.cPot

    lift $ writeIORef (s^.qmlState.potChipsS) pot
    lift $ fireSignal (s^.qmlState.potChipsSig) (s^.ctx)

updateCards :: CGameStateT ()
updateCards = do
    s <- get

    let cards = s^..game.cPlayers.traversed.cCards
        paddedPCards = map cardToFileNameTexts $ padMaxP cards []
        tableCards = map cardToFileNameText $ s^.game.cCommunityCards
        paddedTCards = padTCards tableCards cardBack

    lift $ writeIORef (s^.qmlState.pCardsS) paddedPCards
    lift $ fireSignal (s^.qmlState.pCardsSig) (s^.ctx)

    lift $ writeIORef (s^.qmlState.tCardsS) paddedTCards
    lift $ fireSignal (s^.qmlState.tCardsSig) (s^.ctx)

updateVisible :: CGameStateT ()
updateVisible = do
    s <- get

    let visible = replicate (length $ s^.game.cPlayers) True
        padded = padMaxP visible False

    lift $ writeIORef (s^.qmlState.pVisibleS) padded
    lift $ fireSignal (s^.qmlState.pVisibleSig) (s^.ctx)

updateButtons :: [Bool] -> CGameStateT ()
updateButtons bs = do
    s <- get 

    lift $ writeIORef (s^.qmlState.bEnabledS) bs
    lift $ fireSignal (s^.qmlState.bEnabledSig) (s^.ctx)

updateCurrentPlayer :: CGameStateT ()
updateCurrentPlayer = do
    s <- get

    let index = fromJust $ elemIndex (s^.game.cCurrentPlayer)
                                     (s^..game.cPlayers.traversed.cUUID)

        bools = replicate index False ++ [True]
        padded = padMaxP bools False

    lift $ writeIORef (s^.qmlState.pCurrentPlayerS) padded
    lift $ fireSignal (s^.qmlState.pCurrentPlayerSig) (s^.ctx)

padMaxP :: [a] -> a -> [a]
padMaxP = pad maxPlayers

padTCards :: [a] -> a -> [a]
padTCards = pad numTCards

pad :: Int -> [a] -> a -> [a]
pad maxLen xs def
    | length xs > maxLen = error "Too long list passed to pad!"
    | otherwise = xs ++ replicate (maxLen - len) def
    where len = length xs

cardToFileNameTexts :: [Card] -> [Text]
cardToFileNameTexts cs = case cs of
    [] -> [cardBack, cardBack]
    [a, b] -> [cardToFileNameText a, cardToFileNameText b]
    _ -> error "Invalid cards passed to cardToFileNameTexts!"

updateRaiseWindow :: CGameStateT ()
updateRaiseWindow = do
    s <- get

    let me = head $ filter (^.cIsMe) (s^.game.cPlayers)
        minBet = s^.game.cBets.cCurrentBet + s^.game.cBets.cMinimumRaise

    lift $ writeIORef (s^.qmlState.slideMinS) minBet
    lift $ writeIORef (s^.qmlState.slideMaxS) (me^.cBet + me^.cChips)

    lift $ fireSignal (s^.qmlState.slideMinSig) (s^.ctx)
    lift $ fireSignal (s^.qmlState.slideMaxSig) (s^.ctx)

showGameOverWindow :: Lens' StatesNSignals (SignalKey (IO ())) ->
                      Lens' StatesNSignals (IORef Bool) ->
                      CGameStateT ()
showGameOverWindow windowSig windowState = do
    s <- get

    lift $ writeIORef (s^.qmlState.windowState) True
    lift $ fireSignal (s^.qmlState.windowSig) (s^.ctx)

createConsoleNewLine :: CGameStateT ()
createConsoleNewLine = appendTextBox [empty]

updateTextBox :: [Text] -> CGameStateT ()
updateTextBox = appendTextBox

appendTextBox :: [Text] -> CGameStateT ()
appendTextBox msg = do
    s <- get

    lift $ atomicModifyIORef (s^.qmlState.logMsgS) (\x -> (x ++ msg, ()))

    lift $ fireSignal (s^.qmlState.logMsgSig) (s^.ctx)
