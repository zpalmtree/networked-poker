module DrawCard
(
    initDeckKnuth,
    initDeckRandomIndex,
    drawKnuth,
    drawRandomIndex,
    dealHandKnuth,
    dealHandRandomIndex
)
where

import System.Random (getStdRandom, randomR)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Lens ((^.), (.=))
import System.Log.Logger (infoM)

import Lenses (cardInfo, deck)

import Types 
    (Card(..), Suit(..), Value(..), KnuthDeck(..), GameStateT, 
     RandomIndexDeck(..), Deck(..))

initDeckKnuthPure :: IO Deck
initDeckKnuthPure = do
    deck' <- shuffle (length fullDeck - 1) fullDeck
    
    return $ IsKnuth $ KnuthDeck deck'

initDeckKnuth :: GameStateT ()
initDeckKnuth = do
    lift $ infoM "Prog.initDeckKnuth" "Using knuth shuffle"

    deck' <- lift $ shuffle (length fullDeck -1) fullDeck

    cardInfo.deck .= (IsKnuth $ KnuthDeck deck')

shuffle :: Int -> [a] -> IO [a]
shuffle 0 xs = return xs
shuffle i xs = do
    j <- getStdRandom $ randomR (0, i)
    shuffle (i-1) (swap i j xs)
    
drawKnuthPure :: Deck -> (Card, Deck)
drawKnuthPure (IsKnuth (KnuthDeck (card:deck'))) = 
    (card, IsKnuth $ KnuthDeck deck')

drawKnuthPure _ = error "Expected IsKnuth deck!"

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

initDeckRandomIndexPure :: Deck
initDeckRandomIndexPure = IsRandomIndex $ RandomIndexDeck fullDeck

initDeckRandomIndex :: GameStateT ()
initDeckRandomIndex = do
    lift $ infoM "Prog.initDeckKnuth" "Using randomIndex shuffle"

    cardInfo.deck .= (IsRandomIndex $ RandomIndexDeck fullDeck)

drawRandomIndexPure :: Deck -> IO (Card, Deck)
drawRandomIndexPure (IsRandomIndex (RandomIndexDeck cards)) = do
    randomNum <- getStdRandom $ randomR (0, length cards - 1) 

    let (beginning, card:end) = splitAt randomNum cards

    return (card, IsRandomIndex $ RandomIndexDeck $ beginning ++ end)

drawRandomIndexPure _ = error "Expected IsRandomIndex deck!"

drawRandomIndex :: GameStateT Card
drawRandomIndex = do
    s <- get

    let (IsRandomIndex (RandomIndexDeck cards)) = s^.cardInfo.deck

    randomNum <- lift . getStdRandom $ randomR (0, length cards - 1)

    let (beginning, card:end) = splitAt randomNum cards

    cardInfo.deck .= (IsRandomIndex . RandomIndexDeck $ beginning ++ end)

    return card

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [Two .. Ace],
                              suit  <- [Heart .. Diamond]]

dealHandKnuth :: IO [Card]
dealHandKnuth = do
    deck' <- initDeckKnuthPure

    return $ draw handSize deck'

    where draw 0 _ = []
          draw n deck' = let (card, newDeck) = drawKnuthPure deck'
                         in  card : draw (n-1) newDeck

dealHandRandomIndex :: IO [Card]
dealHandRandomIndex = do
    let deck' = initDeckRandomIndexPure

    draw handSize deck'

    where draw 0 _ = return []
          draw n deck' = do
            (card, newDeck) <- drawRandomIndexPure deck'
            rest <- draw (n-1) newDeck
            return $ card : rest

-- 6 players with 2 cards each, 5 table cards
handSize :: (Num a) => a
handSize = (6 * 2) + 5
