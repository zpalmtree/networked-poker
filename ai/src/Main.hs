module Main
(
    main
)
where

import Numeric.LinearAlgebra (fromList)
import Data.Char (digitToInt)
import System.Random (getStdRandom, randomR)
import Control.Monad (replicateM)
import Text.Printf (printf)

import AI.HNN.FF.Network 
    (Sample, (-->), createNetwork, output, trainNTimes, tanh')

import Paths_ai (getDataFileName)

main :: IO ()
main = do
    net <- createNetwork 2 [2] 1

    evS <- readEV

    samples <- replicateM 4 (createSample evS)

    putStrLn "Pretrain values:"

    mapM_ (\x -> printf "%s = %s\n" (show x) 
                (show (output net tanh $ fst x))) samples

    putStrLn ""

    let smartNet = trainNTimes 100000 0.8 tanh tanh' net samples

    putStrLn "Post train values:"
    
    --post train values
    mapM_ (\x -> printf "%s = %s\n" (show x) 
                (show (output smartNet tanh $ fst x))) samples

createSample :: [EV] -> IO (Sample Float)
createSample ev' = do
    let xs = [1..52]

    (num, new) <- takeNum xs
    (num2, _) <- takeNum new

    let ev'' = evPreFlop num num2 ev'

    return $ fromList [fromIntegral num, fromIntegral num2] --> fromList [ev'']

takeNum :: [Int] -> IO (Int, [Int])
takeNum xs = do
    randomNum <- getStdRandom $ randomR (0, length xs - 1)

    let (beginning, n:end) = splitAt randomNum xs

    return (n, beginning ++ end)

-- 1  -> 13 = Ace, Two .. King
-- 1  -> 13 = Heart
-- 14 -> 26 = Diamond
-- 27 -> 39 = Spade
-- 40 -> 52 = Club
evPreFlop :: Int -> Int -> [EV] -> Float
evPreFlop card1 card2 ev' = normalise $ 
    calcEV (cardValue card1) (cardValue card2) (sameSuit card1 card2) ev'

sameSuit :: (Integral a, Integral b) => a -> b -> Bool
sameSuit card1 card2 = ceiling (c1 / 13) == ceiling (c2 / 13)
    where c1 = fromIntegral card1
          c2 = fromIntegral card2

cardValue :: Integral a => a -> a
cardValue card
    | val == 0 = 13
    | otherwise = val
    where val = card `rem` 13

calcEV :: Int -> Int -> Bool -> [EV] -> Float
calcEV card1val card2val sameSuit' evS = ev x
    where (x:_) = filter (correctCards card1val card2val sameSuit') evS

normalise :: Fractional a => a -> a
normalise n = (n - minEV) / (maxEV - minEV)
    where maxEV = 2.32
          minEV = -0.15

readEV :: IO [EV]
readEV = do
    fileLoc <- getDataFileName "src/EVs.txt"
    input <- lines <$> readFile fileLoc
    let cardEVPair = splitEvery 2 input
    return $ map makeEV cardEVPair

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
    where (first, rest) = splitAt n list

data EV = EV {
    cardValues :: [Int],
    bothSameSuit :: Bool,
    ev :: Float
}

makeEV :: [String] -> EV
makeEV [x, y] = EV values sameSuit' ev'
    where ev' = read y
          sameSuit' = 's' `elem` x
          values = parseValues x

makeEV _ = error "Incorrect format passed to makeEV"

parseValues :: String -> [Int]
parseValues (x:y:_) = [parseValue x, parseValue y]
parseValues _ = error "Incorrect format passed to parseValues"

parseValue :: Char -> Int
parseValue 'A' = 1
parseValue 'K' = 13
parseValue 'Q' = 12
parseValue 'J' = 11
parseValue 'T' = 10
parseValue n = digitToInt n

correctCards :: Int -> Int -> Bool -> EV -> Bool
correctCards card1val card2val sameSuit' ev' = 
    card1val `elem` cardValues ev' &&
    card2val `elem` cardValues ev' &&
    sameSuit' == bothSameSuit ev'
