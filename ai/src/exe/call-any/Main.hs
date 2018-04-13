module Main
(
    main
)
where

import AIFramework (runAI)
import AICallAny (handleFunc)

main :: IO ()
main = runAI "call-any" handleFunc
