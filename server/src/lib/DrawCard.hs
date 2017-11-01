module DrawCard
(
    initDeckKnuth,
    initDeckRandomIndex,
    drawKnuth,
    drawRandomIndex
)
where

import System.Random (getStdRandom, randomR)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Lens ((^.), (.=))
import System.Log.Logger (infoM)

import Types (Card(..), Suit(..), Value(..), KnuthDeck(..), 
              RandomIndexDeck(..), Deck(..), GameStateT)

import Lenses (cardInfo, deck)

initDeckKnuth :: GameStateT ()
initDeckKnuth = do
    lift $ infoM "Prog.initDeckKnuth" "Using knuth shuffle"

    deck' <- lift $ shuffle (length fullDeck -1) fullDeck

    cardInfo.deck .= (IsKnuth $ KnuthDeck deck')

    where shuffle 0 xs = return xs
          shuffle i xs = do
            j <- getStdRandom $ randomR (0, i)
            shuffle (i-1) (swap i j xs)

drawKnuth :: GameStateT Card
drawKnuth = do
    s <- get

    let (IsKnuth (KnuthDeck (card:deck'))) = s^.cardInfo.deck

    cardInfo.deck .= (IsKnuth $ KnuthDeck deck')

    return card

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

initDeckRandomIndex :: GameStateT ()
initDeckRandomIndex = do
    lift $ infoM "Prog.initDeckKnuth" "Using randomIndex shuffle"

    cardInfo.deck .= (IsRandomIndex $ RandomIndexDeck fullDeck)

drawRandomIndex :: GameStateT Card
drawRandomIndex = do
    s <- get

    let (IsRandomIndex (RandomIndexDeck cards)) = s^.cardInfo.deck

    randomNum <- lift . getStdRandom $ randomR (0, length cards- 1)

    let (beginning, card:end) = splitAt randomNum cards

    cardInfo.deck .= (IsRandomIndex . RandomIndexDeck $ beginning ++ end)

    return card

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [Two .. Ace],
                              suit  <- [Heart .. Diamond]]
