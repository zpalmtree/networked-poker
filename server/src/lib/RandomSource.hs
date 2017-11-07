module RandomSource
(
    randomFrom0ToN_LEucyer,
    randomFrom0ToN_Mersenne,
    randomFrom0ToN_MWC256
)
where

import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR)
import System.Random.MWC (Seed, createSystemRandom, uniformR, save, restore)
import qualified System.Random.Mersenne as M (getStdRandom, random)

import Control.Concurrent.MVar 
    (MVar, newEmptyMVar, putMVar, modifyMVar, isEmptyMVar)

-- the default implementation from System.Random, with a cycle of 2.30584e18
randomFrom0ToN_LEucyer :: Int -> IO Int
randomFrom0ToN_LEucyer n = getStdRandom $ randomR (0, n)

-- Uses a Fast Mersenne Twister, with a cycle of 2^19937-1
randomFrom0ToN_Mersenne :: Int -> IO Int
randomFrom0ToN_Mersenne n 
    | n == 0 = error "Divide by zero in randomFrom0ToN_Mersenne"
    | otherwise = (`mod` n) <$> M.getStdRandom M.random

-- Uses Marsaglia's MWC256, with a cycle of 2^8222
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
