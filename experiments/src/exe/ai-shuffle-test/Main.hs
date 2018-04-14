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
import LaunchServer (launchServerWithShuffle)
import AIFramework (runAI)

import Types
    (DrawAlgorithm(..), Deck(..), ShuffleType, RandomIndexDeck(..), 
     KnuthDeck(..), RandomSortDeck(..), ShuffleType(..))

data WinRate = WinRate {
    callAny :: Int,
    ruleBased :: Int
}

main :: IO ()
main = do
    rng <- getRng

    algo <- getAlgo rng
    let deck = mkDeck rng

    serverThread <- forkIO $ launchServerWithShuffle algo deck

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
