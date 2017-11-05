{-# LANGUAGE FlexibleContexts #-}

module DrawCard
(
    getInitFunc,
    getDrawFunc,
    getRNGFunc,
    dealHand,
    initM,
    drawM,
    initDeckKnuth,
    randomFrom0ToN_LEucyer
)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Lens ((.=), (^.))
import System.Random (getStdRandom, randomR)
import qualified System.Random.Mersenne as M (getStdRandom, random)

import Lenses (cardInfo, deck)

import Types 
    (KnuthDeck(..), Deck(..), Card(..), Value(..), RandomIndexDeck(..),
     Suit(..), GameStateT, RandomSource(..), DrawAlgorithm(..))

-- DEAL WHOLE HAND

dealHand :: (a -> IO b) -> (a -> b -> IO (c, b)) -> a -> IO [c]
dealHand initFunc drawFunc randomSource = do
    x <- initFunc randomSource

    draw handSize x

    where draw 0 _ = return []
          draw n xs = do
            (card, newDeck) <- drawFunc randomSource xs
            rest <- draw (n-1) newDeck
            return $ card : rest

          handSize = (6 * 2) + 5

-- MONADIC INIT AND DRAW

initM :: (a -> IO Deck) -> a -> GameStateT ()
initM initFunc randomSource = do
    x <- lift $ initFunc randomSource
    cardInfo.deck .= x

drawM :: (a -> Deck -> IO (b, Deck)) -> a -> GameStateT b
drawM drawFunc randomSource = do
    s <- get
    (card, x) <- lift $ drawFunc randomSource (s^.cardInfo.deck)
    cardInfo.deck .= x
    return card

-- RANDOM NUMBER IMPLEMENTATIONS

-- the default implementation from System.Random, with a cycle of 2.30584e18
randomFrom0ToN_LEucyer :: Int -> IO Int
randomFrom0ToN_LEucyer n = getStdRandom $ randomR (0, n)

-- Uses a Fast Mersenne Twister, with a cycle of 2^19937-1
randomFrom0ToN_Mersenne :: Int -> IO Int
randomFrom0ToN_Mersenne n = do
    result <- M.getStdRandom M.random
    return $ result `mod` n

-- INIT FUNCS

initDeckKnuth :: (Int -> IO Int) -> IO Deck
initDeckKnuth randomSource = do
    x <- shuffle randomSource (length fullDeck - 1) fullDeck
    return . IsKnuth $ KnuthDeck x

initDeckRandomIndex :: (Int -> IO Int) -> IO Deck
initDeckRandomIndex _ = return . IsRandomIndex $ RandomIndexDeck fullDeck

-- DRAW FUNCS

drawKnuth :: (Int -> IO Int) -> Deck -> IO (Card, Deck)
drawKnuth _ (IsKnuth (KnuthDeck (card:deck'))) = return
    (card, IsKnuth $ KnuthDeck deck')

drawKnuth _ _ = error "Expected IsKnuth deck!"

drawRandomIndex :: (Int -> IO Int) -> Deck -> IO (Card, Deck)
drawRandomIndex randomSource (IsRandomIndex (RandomIndexDeck cards)) = do
    randomNum <- randomSource $ length cards - 1

    let (beginning, card:end) = splitAt randomNum cards

    return (card, IsRandomIndex . RandomIndexDeck $ beginning ++ end)

drawRandomIndex _ _ = error "Expected IsRandomIndex deck!"

-- UTILITIES

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [Two .. Ace],
                              suit  <- [Heart .. Diamond]]

shuffle :: (Int -> IO Int) -> Int -> [a] -> IO [a]
shuffle _ 0 xs = return xs
shuffle randomSource i xs = do
    j <- randomSource i
    shuffle randomSource (i-1) (swap i j xs)

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

getRNGFunc :: RandomSource -> (Int -> IO Int)
getRNGFunc LEucyer = randomFrom0ToN_LEucyer
getRNGFunc Mersenne = randomFrom0ToN_Mersenne

getDrawFunc :: DrawAlgorithm -> ((Int -> IO Int) -> Deck -> IO (Card, Deck))
getDrawFunc Knuth = drawKnuth
getDrawFunc RandomIndex = drawRandomIndex

getInitFunc :: DrawAlgorithm -> (Int -> IO Int) -> IO Deck
getInitFunc Knuth = initDeckKnuth
getInitFunc RandomIndex = initDeckRandomIndex
