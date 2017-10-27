module DrawCard
(
    StandardDraw(),
    ShuffleDraw(),
    initDeck,
    draw
)
where

import System.Random (getStdRandom, randomR)
import Data.Coerce (coerce)

import Types (Card)
import Utilities.Card (fullDeck)

newtype Deck = Deck [Card]

class Drawable a where
    initDeck :: a
    draw :: a -> IO (a, Card)

newtype StandardDraw = StandardDraw Deck

instance Drawable StandardDraw where
    initDeck = coerce fullDeck

    draw x = do
        let cards = coerce x

        randomNum <- getStdRandom $ randomR (0, length cards- 1)

        let (beginning, card:end) = splitAt randomNum cards

        return (coerce $ beginning ++ end, card)

newtype ShuffleDraw = ShuffleDraw Deck

instance Drawable ShuffleDraw where
    initDeck = coerce $ shuffle fullDeck
        where shuffle :: a -> a
              shuffle = undefined

    draw x = return (coerce deck, card)
        where (card:deck) = coerce x
