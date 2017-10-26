{-# LANGUAGE RankNTypes #-}

module DrawCard
(
    StandardDraw(),
    initDeck,
    draw
)
where

import System.Random (getStdRandom, randomR)

import Types (Card)
import Utilities.Card (fullDeck)

newtype Deck = Deck { getCard :: [Card] }

class Drawable a where
    initDeck :: a
    draw :: a -> IO (a, Card)
    unwrap :: a -> [Card]
    wrap :: [Card] -> a

newtype StandardDraw = StandardDraw { getStandardDraw :: Deck }

instance Drawable StandardDraw where
    initDeck = wrap fullDeck

    draw x = do
        let cards = unwrap x

        randomNum <- getStdRandom $ randomR (0, length cards- 1)

        let (beginning, card:end) = splitAt randomNum cards

        return (wrap $ beginning ++ end, card)

    unwrap = getCard . getStandardDraw

    wrap = StandardDraw . Deck

newtype ShuffleDraw = ShuffleDraw { getShuffleDraw :: Deck }

instance Drawable ShuffleDraw where
    initDeck = wrap $ shuffle fullDeck
        where shuffle = undefined

    draw x = return (wrap deck, card)
        where (card:deck) = unwrap x

    unwrap = getCard . getShuffleDraw

    wrap = ShuffleDraw . Deck
