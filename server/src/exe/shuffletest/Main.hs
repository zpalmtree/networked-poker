module Main
(
    main
)
where

import Data.Map.Lazy (Map, insertWith, empty)

import DrawCard (dealHandKnuth)
import Types (Card)

main :: IO ()
main = do
    mapping <- dealNHands 10000 dealHandKnuth empty
    print mapping

dealNHands :: Int -> IO [Card] -> Map Card Int -> IO (Map Card Int)
dealNHands 0 _ acc = return acc
dealNHands n f acc = do
    hand <- f
    dealNHands (n-1) f $ updateAccumulator acc hand

updateAccumulator :: Map Card Int -> [Card] -> Map Card Int
updateAccumulator acc [] = acc
updateAccumulator acc (x:xs) = updateAccumulator (insertWith (+) x 1 acc) xs
