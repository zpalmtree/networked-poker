module DrawCard
(
    Deck(),
    initDeck,
    drawCard
)
where

import System.Random (getStdRandom, randomR)

import Types (Card)
import Utilities.Card (fullDeck)

newtype Deck = Deck { getDeck :: [Card] }

initDeck :: Deck
initDeck = initDeckDefault

drawCard :: Deck -> IO (Deck, Card)
drawCard deck = do
    (newDeck, card) <- drawFunction $ getDeck deck
    return (Deck newDeck, card)

-- reimplement this
drawFunction :: [Card] -> IO ([Card], Card)
drawFunction = drawCard1
--drawFunction = drawCard2

initDeckDefault :: Deck
initDeckDefault = Deck generateDeckFunction

-- reimplement this
generateDeckFunction :: [Card]
generateDeckFunction = fullDeck
--generateDeckFunction = undefined

-- creates random number in deck range and takes that card
-- returns new deck
drawCard1 :: [Card] -> IO ([Card], Card)
drawCard1 deck = do
    randomNum <- getStdRandom $ randomR (0, length deck - 1) 
    let (beginning, card:end) = splitAt randomNum deck
    return (beginning ++ end, card)
