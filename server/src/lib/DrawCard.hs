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

import Types (Card(..), Value(..), Suit(..))

-- These functions don't check that they aren't passed empty lists for
-- demonstration simplicity

newtype Deck = Deck [Card]

class Drawable a where
    initDeck :: IO a
    draw :: a -> IO (a, Card)

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [Two .. Ace],
                              suit  <- [Heart .. Diamond]]

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

-- adapted from https://stackoverflow.com/a/30551130/8737306
swap :: Int -> Int -> [a] -> [a]
swap i j xs
    | i == j = xs
    | otherwise = let elemI = xs !! i
                      elemJ = xs !! j
                      left = take j xs
                      middle = take (i - j - 1) (drop (j + 1) xs)
                      right = drop (i + 1) xs
                  in  left ++ [elemI] ++ middle ++ [elemJ] ++ right
