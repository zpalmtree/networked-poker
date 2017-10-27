module DrawCard
(
    Drawable,
    RandomIndex(),
    KnuthShuffle(),
    initDeck,
    draw
)
where

import System.Random (getStdRandom, randomR)
import Data.Coerce (coerce)

import Types (Card)
import Utilities.Card (fullDeck)

-- These functions don't check that they aren't passed empty lists for
-- demonstration simplicity

newtype Deck = Deck [Card]

class Drawable a where
    initDeck :: IO a
    draw :: a -> IO (a, Card)

newtype RandomIndex = RandomIndex Deck

instance Drawable RandomIndex where
    initDeck = return $ coerce fullDeck

    draw x = do
        let cards = coerce x

        randomNum <- getStdRandom $ randomR (0, length cards- 1)

        let (beginning, card:end) = splitAt randomNum cards

        return (coerce $ beginning ++ end, card)

newtype KnuthShuffle = KnuthShuffle Deck

instance Drawable KnuthShuffle where
    initDeck = coerce $ shuffle (length fullDeck - 1) fullDeck
        where shuffle 0 xs = return xs
              shuffle i xs = do
                j <- getStdRandom $ randomR (0, i)
                shuffle (i-1) (swap i j xs)

    draw x = return (coerce deck, card)
        where (card:deck) = coerce x

swap :: Int -> Int -> [a] -> [a]
swap a b 
    | a == b = id
    | otherwise = swap' (min a b) (max a b)
    where swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
            where (beginning, (x : r)) = splitAt first lst
                  (middle, (y : end)) = splitAt (second - first - 1) r
