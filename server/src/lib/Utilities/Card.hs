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

import System.Random (getStdRandom, randomR)
import Control.Lens ((^.), (.=), (%=), (<~), ix)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, replicateM_, replicateM)
import Safe (at)

import Types (Card(..), Value(..), Suit(..), Stage(..), GameStateT)
import Utilities.Player (numPlayers)
import Output (outputPlayerCards)

import Lenses 
    (cards, cardInfo, deck, tableCards, stage, playerQueue, players)

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

    let deck' = s^.cardInfo.deck

    when (null deck') $ error "Can't take a card from an empty deck!"

    cardNum <- lift . getStdRandom $ randomR (0, length deck' - 1)
    deleteNth cardNum

    return $ deck' `at` cardNum

drawCard :: GameStateT ()
drawCard = do
    card <- getRandomCard
    cardInfo.tableCards %= (\cards' -> cards' ++ [card])

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

deleteNth :: Int -> GameStateT ()
deleteNth n = do
    when (n < 0) $ error "Can't remove negative deck index!"

    cardInfo.deck %= (\xs -> take n xs ++ drop (n+1) xs)

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
