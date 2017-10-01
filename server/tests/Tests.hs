module Main
(
    main
)
where

import BettingTests as BT (runTests)
import ShowdownTests as ST (runTests)

import System.Exit (ExitCode, exitSuccess, exitFailure)

main :: IO ExitCode
main = do
    good <- and <$> sequence [BT.runTests, ST.runTests]
    if good
        then exitSuccess
        else exitFailure
