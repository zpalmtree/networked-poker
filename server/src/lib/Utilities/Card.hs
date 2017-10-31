module Utilities.Card
(
    dealCards,
    hearts,
    clubs,
    diamonds,
    spades,
    fullDeck,
    revealFlop,
    revealTurn,
    revealRiver,
)
where

import Control.Lens ((^.), (.=), (%=), (<~), ix)
import Control.Monad.Trans.State (get)
import Control.Monad (replicateM_, replicateM)

import Utilities.Player (numPlayers)
import Output (outputPlayerCards)
import DrawCard (drawKnuth, drawRandomIndex)

import Types 
    (Card(..), Value(..), Suit(..), Stage(..), GameStateT, ShuffleType(..))

import Lenses 
    (cards, cardInfo, tableCards, stage, playerQueue, players, shuffleType)

dealCards :: GameStateT ()
dealCards = do
    updateCards =<< numPlayers
    outputPlayerCards

updateCards :: Int -> GameStateT ()
updateCards 0 = return ()
updateCards n = do
    playerQueue.players.ix (n-1).cards <~ drawPlayerCards
    updateCards (n-1)

drawPlayerCards :: GameStateT [Card]
drawPlayerCards = replicateM 2 getRandomCard

getRandomCard :: GameStateT Card
getRandomCard = do
    s <- get

    case s^.shuffleType of
        Knuth -> drawKnuth
        RandomIndex -> drawRandomIndex

drawCard :: GameStateT ()
drawCard = do
    card <- getRandomCard
    cardInfo.tableCards %= (++ [card])

revealFlop :: GameStateT ()
revealFlop = do
    replicateM_ 3 drawCard
    stage .= Flop

revealTurn :: GameStateT ()
revealTurn = do
    drawCard
    stage .= Turn

revealRiver :: GameStateT ()
revealRiver = do
    drawCard
    stage .= River

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [Two .. Ace],
                              suit  <- [Heart .. Diamond]]

hearts :: [Card]
hearts = [Card value Heart | value <- [minBound :: Value .. maxBound]]

clubs :: [Card]
clubs = [Card value Club | value <- [minBound :: Value .. maxBound]]

diamonds :: [Card]
diamonds = [Card value Diamond | value <- [minBound :: Value .. maxBound]]

spades :: [Card]
spades = [Card value Spade | value <- [minBound :: Value .. maxBound]]
