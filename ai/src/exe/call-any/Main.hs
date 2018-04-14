module Main
(
    main
)
where

import AIFramework (runAI)
import AICallAny (handleFunc)

main :: IO ()
main = do
    result <- runAI "call-any" handleFunc

    case result of
        True -> putStrLn "AI-Call-Any won!"
        False -> putStrLn "AI-Call-Any lost!"
