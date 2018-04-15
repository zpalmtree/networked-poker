module Main
(
    main
)
where

import System.IO (hFlush, stdout)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (concurrently)
import Text.Printf (printf)

import qualified AICallAny as CA (handleFunc)
import qualified AIRuleBased as RB (handleFunc)
import Utilities.Card (fullDeck)
import DrawCard (getInitFunc, getRNGFunc)
import LaunchServer (launchServerWithShuffle)
import AIFramework (runAI)

import Types
    (DrawAlgorithm(..), Deck(..), ShuffleType, RandomIndexDeck(..), 
     KnuthDeck(..), RandomSortDeck(..), ShuffleType(..), RandomSource(..))

data WinRate = WinRate {
    callAny :: Int,
    ruleBased :: Int
}

main :: IO ()
main = do
    algo <- getAlgo
    rng <- getRng

    let shuffleType = ShuffleType algo rng

    serverThread <- forkIO $ launchServerWithShuffle shuffleType

    putStrLn "Launching server..."

    -- wait for server to launch
    threadDelay 5000000

    putStrLn "Server started!"

    winRates <- playLoop 1 (WinRate 0 0)

    killThread serverThread

    printf "Rule based AI won %d out of %d encounters\n" (ruleBased winRates)
                                                         totalRuns

totalRuns = 1000

playLoop :: Int -> WinRate -> IO WinRate
playLoop n winrate
    | n > totalRuns = return winrate
    | otherwise = do
        printf "Executing game %d of %d\n" n totalRuns

        (callAnyResult, ruleBasedResult) <- concurrently 
                                            (runAI "call-any" CA.handleFunc)
                                            (runAI "rule-based" RB.handleFunc)

        playLoop (n+1) (if callAnyResult
                            then winrate { callAny = callAny winrate + 1 }
                            else winrate { ruleBased = ruleBased winrate + 1 })

mkDeck :: RandomSource -> DrawAlgorithm -> IO Deck
mkDeck rng algo = getInitFunc algo (getRNGFunc rng)

getRng :: IO RandomSource
getRng = getInput "Random Source? [LEucyer, Mersenne, MWC256]: "

getAlgo :: IO DrawAlgorithm
getAlgo = getInput "Draw algorithm? [RandomIndex, Knuth, RandomSort]: "

getInput :: Read a => String -> IO a
getInput msg = do
    putStr msg
    hFlush stdout
    input <- readMaybe <$> getLine

    case input of
        Nothing -> putStrLn "Failed to parse. Try again." >> getInput msg
        Just x -> return x

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
