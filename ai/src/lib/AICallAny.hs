module AICallAny
(
    handleFunc
)
where

import AITypes (AIGameStateT)
import Types (Action(..))

handleFunc :: [Action Int] -> AIGameStateT (Maybe (Action Int))
handleFunc options
    | Call `elem` options = return $ Just Call
    | Check `elem` options = return $ Just Check
    | AllIn `elem` options = return $ Just AllIn
    | otherwise = error "Unexpected lack of options"
