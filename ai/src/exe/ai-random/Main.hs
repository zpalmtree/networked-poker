module Main
(
    main
)
where

import AIFramework (runAI)
import AIRandom (handleFunc)

main :: IO ()
main = do
    result <- runAI "ai-random" handleFunc

    case result of
        True -> putStrLn "AI-Random won!"
        False -> putStrLn "AI-Random lost!"
