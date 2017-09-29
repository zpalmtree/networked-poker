module Main
(
    main
)
where

import BettingTests as BT (runTests)

import System.Exit (ExitCode, exitSuccess, exitFailure)

main :: IO ExitCode
main = do
    good <- and <$> sequence [BT.runTests]
    if good
        then exitSuccess
        else exitFailure
