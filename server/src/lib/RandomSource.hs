module RandomSource
(
    randomFrom0ToN_LEucyer,
    randomFrom0ToN_Mersenne,
    randomFrom0ToN_MWC256,
    probOfUnbiasedShuffle
)
where

import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR)
import System.Random.MWC (Seed, createSystemRandom, uniformR, save, restore)
import Data.Word (Word32)
import Data.Number.CReal (showCReal)
import qualified System.Random.Mersenne as M (getStdRandom, random)

import Control.Concurrent.MVar 
    (MVar, newEmptyMVar, putMVar, modifyMVar, isEmptyMVar)

randomFrom0ToN_LEucyer :: Int -> IO Int
randomFrom0ToN_LEucyer n = getStdRandom $ randomR (0, n)

-- modified from https://stackoverflow.com/a/17554531/8737306
randomFrom0ToN_Mersenne :: Int -> IO Int
randomFrom0ToN_Mersenne n = do
    let range = 1 + fromIntegral n
        buckets = maxBound `div` range
        limit = buckets * range

    r <- getValidValue limit

    return . fromIntegral $ r `div` buckets

getValidValue :: Word32 -> IO Word32
getValidValue limit = do
    r <- M.getStdRandom M.random
    if r >= limit
        then getValidValue limit
        else return r

randomFrom0ToN_MWC256 :: Int -> IO Int
randomFrom0ToN_MWC256 n = do
    empty <- isEmptyMVar genSeed

    -- no seed yet so create it
    when empty $ do
        gen <- createSystemRandom
        seed <- save gen
        putMVar genSeed seed

    -- get seed, restore gen, create number, get new seed, store new seed
    modifyMVar genSeed $ \seed -> do
        gen <- restore seed
        x <- uniformR (0, n) gen
        newSeed <- save gen
        return (newSeed, x)

-- I seriously hope you guys don't do this

-- Ok so I want all my random functions to take an Int and return an IO Int.
-- But MWC gives back a seed and takes a seed to restore/save the gen.
-- If we don't save it it's crazy expensive. But then we need to take and 
-- return a seed which means it's not compatible with the other functions. 
-- So we just use a global MVar which is pretty ugly but it keeps everything 
-- working fine.
{-# NOINLINE genSeed #-}
genSeed :: MVar Seed
genSeed = unsafePerformIO newEmptyMVar

probOfUnbiasedShuffle :: Integer -> String
probOfUnbiasedShuffle k = showCReal 100 prob
    where prob = 1 - (numerator / denominator)
          numerator = product $ map (\n -> k2 - n) [0..51]
          denominator = k2^52
          k2 = (2^k) - 1
