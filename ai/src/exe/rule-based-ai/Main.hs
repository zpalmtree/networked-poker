module Main
(
    main
)
where

import AIFramework (runAI)
import AIRuleBased (handleFunc)

main :: IO ()
main = do
    result <- runAI "rule-based-ai" handleFunc

    case result of
        True -> putStrLn "AI-Rule-Based won!"
        False -> putStrLn "AI-Rule-Based lost!"
