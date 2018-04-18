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
import Control.Monad (replicateM_, replicateM, forM_)

import Utilities.Player (numPlayers)
import Output (outputPlayerCards)
import DrawCard (drawM, getRNGFunc, getDrawFunc)
import Types  (Card(..), Value(..), Suit(..), Stage(..), GameStateT)

import Lenses 
    (cards, cardInfo, tableCards, stage, playerQueue, players, shuffleType,
     algorithm, randomSource)

dealCards :: GameStateT ()
dealCards = do
    updateCards =<< numPlayers
    outputPlayerCards

updateCards :: Int -> GameStateT ()
updateCards n = forM_ [0 .. n - 1] $ 
    \x -> playerQueue.players.ix x.cards <~ drawPlayerCards

drawPlayerCards :: GameStateT [Card]
drawPlayerCards = replicateM 2 getRandomCard

getRandomCard :: GameStateT Card
getRandomCard = do
    s <- get

    let drawFunc = getDrawFunc $ s^.shuffleType.algorithm
        rngFunc = getRNGFunc $ s^.shuffleType.randomSource

    drawM drawFunc rngFunc

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
