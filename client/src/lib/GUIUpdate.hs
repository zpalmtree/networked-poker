module GUIUpdate
(
    updateNames,
    updateBets,
    updateCards,
    updateVisible,
    updateInPlay,
    updateButtons,
    updateCurrentPlayer,
    updatePot
)
where

import Data.Text (Text, pack)
import Control.Lens ((^.), (^..), traversed)
import Graphics.QML (fireSignal)
import Data.IORef (writeIORef)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import Constants (maxPlayers, cardBack)
import ClientTypes (CGameStateT)
import Types (Card)

import Lenses 
    (cPlayers, cName, cChips, cCards, cBet, cInPlay, cCommunityCards,
     cCurrentPlayer, cUUID, cBets, cPot)

import CLenses 
    (game, qmlState, pVisibleS, pVisibleSig, ctx, pNamesS, pNamesSig,
     pBetsS, pBetsSig, pCardsS, pCardsSig, pInPlayS, pInPlaySig, tCardsSig,
     tCardsS, bEnabledSig, bEnabledS, pCurrentPlayerSig, pCurrentPlayerS,
     potChipsS, potChipsSig)

updateInPlay :: CGameStateT ()
updateInPlay = do
    s <- get

    let inPlay = s^..game.cPlayers.traversed.cInPlay
        padded = pad inPlay False

    lift $ writeIORef (s^.qmlState.pInPlayS) padded
    lift $ fireSignal (s^.qmlState.pInPlaySig) (s^.ctx)

updateNames :: CGameStateT ()
updateNames = do
    s <- get

    let names = s^..game.cPlayers.traversed.cName
        chips = s^..game.cPlayers.traversed.cChips
        fullText = zipWith (\a b -> a ++ "\n" ++ show b) names chips
        padded = map pack $ pad fullText ""

    lift $ writeIORef (s^.qmlState.pNamesS) padded
    lift $ fireSignal (s^.qmlState.pNamesSig) (s^.ctx)

updateBets :: CGameStateT ()
updateBets = do
    s <- get

    let bets = s^..game.cPlayers.traversed.cBet
        padded = pad bets 0

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
        paddedPCards = map convertCards $ pad cards []
        tableCards = map convertCard $ s^.game.cCommunityCards
        paddedTCards = pad tableCards cardBack

    lift $ writeIORef (s^.qmlState.pCardsS) paddedPCards
    lift $ fireSignal (s^.qmlState.pCardsSig) (s^.ctx)

    lift $ writeIORef (s^.qmlState.tCardsS) paddedTCards
    lift $ fireSignal (s^.qmlState.tCardsSig) (s^.ctx)

updateVisible :: CGameStateT ()
updateVisible = do
    s <- get

    let visible = replicate (length $ s^.game.cPlayers) True
        padded = pad visible False

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
        padded = pad bools False

    lift $ writeIORef (s^.qmlState.pCurrentPlayerS) padded
    lift $ fireSignal (s^.qmlState.pCurrentPlayerSig) (s^.ctx)

pad :: [a] -> a -> [a]
pad xs def
    | length xs > maxPlayers = error "Too long list passed to pad!"
    | otherwise = xs ++ replicate (maxPlayers - len) def
    where len = length xs

convertCard :: Card -> Text
convertCard c = pack $ show c

convertCards :: [Card] -> [Text]
convertCards cs = case cs of
    [] -> [cardBack, cardBack]
    [a, b] -> [convertCard a, convertCard b]
    _ -> error "Invalid cards passed to convertCards!"
