module Main
(
    main
)
where

import AIFramework (runAI)
import AIRuleBased (handleFunc)

main :: IO ()
main = runAI "rule-based-ai" handleFunc
