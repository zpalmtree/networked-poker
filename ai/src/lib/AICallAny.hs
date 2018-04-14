module AICallAny
(
    handleFunc
)
where

import AITypes (AIGameStateT)
import Types (Action(..))

handleFunc :: [Action Int] -> AIGameStateT (Action Int)
handleFunc options
    | Call `elem` options = return Call
    | Check `elem` options = return Check
    | AllIn `elem` options = return AllIn
    | otherwise = error "Unexpected lack of options"
