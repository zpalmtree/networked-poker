module Main
(
    main
)
where

import System.IO (hFlush, stdout)


import Utilities.Card (fullDeck)
import LaunchServer (launchServerWithShuffle)

import Types
    (DrawAlgorithm(..), Deck(..), ShuffleType, RandomIndexDeck(..), 
     KnuthDeck(..), RandomSortDeck(..), ShuffleType(..))

main :: IO ()
main = do
    rng <- getRng

    algo <- getAlgo rng
    let deck = mkDeck rng

    launchServerWithShuffle algo deck

mkDeck :: DrawAlgorithm -> Deck
mkDeck d = case d of
    RandomIndex -> IsRandomIndex $ RandomIndexDeck fullDeck
    Knuth -> IsKnuth $ KnuthDeck fullDeck
    RandomSort -> IsRandomSort $ RandomSortDeck fullDeck

getAlgo :: DrawAlgorithm -> IO ShuffleType
getAlgo a = do
    putStr "Random Source? [LEucyer, Mersenne, MWC256]: "
    hFlush stdout
    rng <- readMaybe <$> getLine

    case rng of
        Nothing -> putStrLn "Failed to parse. Try again." >> getAlgo a
        Just r -> return $ ShuffleType a r

getRng :: IO DrawAlgorithm
getRng = do
    putStr "Draw algorithm? [RandomIndex, Knuth, RandomSort]: "
    hFlush stdout
    algo <- readMaybe <$> getLine

    case algo of
        Nothing -> putStrLn "Failed to parse. Try again." >> getRng
        Just a -> return a

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
