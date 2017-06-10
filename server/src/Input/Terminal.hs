module Input.Terminal
(
    foldCheckRaise,
    foldCallRaise,
    foldAllIn
)
where

import Types
import Messages

import Data.Char
import Data.Maybe
import Control.Lens hiding (Fold)

{- Things we need to validate here:
- Player makes raise of n, do they actually have n chips?
- Player makes raise of n, is n >= minimum bet?
-}

foldCheckRaise :: Game -> IO (Action Int)
foldCheckRaise game = do
    putStr inputCheck
    input <- map toLower <$> getLine
    case input of
        "fold" -> return Fold
        "check" -> return Check
        "raise" -> getRaiseAmount game
        _ -> putStrLn badCheckInput >> foldCheckRaise game

{- Note: raise amount is new bet value, not current bet + raise.
So, "I want to raise to 500" means if the current bet is 100, the new bet will
be 500, not 600. -}
getRaiseAmount :: Game -> IO (Action Int)
getRaiseAmount game = do
    putStr inputRaise
    input <- maybeRead <$> getLine
    case input of
        Nothing -> putStrLn raiseNotInteger >> getRaiseAmount game
        Just a -> isValidRaise game a

foldCallRaise :: Game -> IO (Action a)
foldCallRaise = undefined

--don't give them option of invalidly raising if they don't have the chips
isValidRaise :: Game -> Int -> IO (Action Int)
isValidRaise game raise'
    | raiseSize >= (game^.bets.minimumRaise) = return $ Raise raise'
    | otherwise = putStrLn invalidRaiseAmount >> getRaiseAmount game
    where raiseSize = raise' - (game^.bets^.currentBet)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

foldAllIn = undefined
