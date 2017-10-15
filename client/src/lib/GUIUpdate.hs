module GUIUpdate
(
    updateNames,
    updateChips,
    updateCards,
    updateVisible
)
where

import Data.Text (Text, pack)
import Control.Lens ((^.), (^..), traversed)
import Graphics.QML (fireSignal)
import Data.IORef (writeIORef)

import Constants (maxPlayers, cardBack)
import ClientTypes (CGame)
import Types (Card)
import Lenses (cPlayerQueue, cPlayers, cName, cChips, cCards)

import CLenses 
    (game, qmlState, pVisibleS, pVisibleSig, ctx, pNamesS, pNamesSig,
     pChipsS, pChipsSig, pCardsS, pCardsSig)

updateNames :: CGame -> IO ()
updateNames cgame = do
    let names = cgame^..game.cPlayerQueue.cPlayers.traversed.cName
        padded = map pack $ pad names ""

    writeIORef (cgame^.qmlState.pNamesS) padded
    fireSignal (cgame^.qmlState.pNamesSig) (cgame^.ctx)

updateChips :: CGame -> IO ()
updateChips cgame = do
    let chips = cgame^..game.cPlayerQueue.cPlayers.traversed.cChips
        padded = pad chips 0

    writeIORef (cgame^.qmlState.pChipsS) padded
    fireSignal (cgame^.qmlState.pChipsSig) (cgame^.ctx)

updateCards :: CGame -> IO ()
updateCards cgame = do
    let cards = cgame^..game.cPlayerQueue.cPlayers.traversed.cCards
        padded = map convertCards $ pad cards []

    writeIORef (cgame^.qmlState.pCardsS) padded
    fireSignal (cgame^.qmlState.pCardsSig) (cgame^.ctx)

updateVisible :: CGame -> IO ()
updateVisible cgame = do
    let visible = replicate (length $ cgame^.game.cPlayerQueue.cPlayers) True
        padded = pad visible False

    writeIORef (cgame^.qmlState.pVisibleS) padded
    fireSignal (cgame^.qmlState.pVisibleSig) (cgame^.ctx)

pad :: [a] -> a -> [a]
pad xs def
    | length xs > maxPlayers = error "Too long list passed to pad!"
    | otherwise = xs ++ replicate (maxPlayers - len) def
    where len = length xs

convertCards :: [Card] -> [Text]
convertCards cs = case cs of
    [] -> [cardBack, cardBack]
    [a, b] -> map pack [show a, show b]
    _ -> error "Invalid cards passed to convertCards!"
