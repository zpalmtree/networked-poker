module Input.Terminal.Input
(
    checkRaiseAllIn,
    checkAllIn,
    foldCallRaiseAllIn,
    foldAllIn,
    foldCallAllIn
)
where

import Types
import Output.Terminal.InputMessages
import PlayerUtilities

import Text.Printf
import Data.Char
import Data.Maybe
import System.IO
import Control.Lens hiding (Fold)

checkRaiseAllIn :: Game -> IO (Action Int)
checkRaiseAllIn game = getAction actionMapping inputCheckRaiseAllIn
                                 badCheckRaiseAllInInput game
    where actionMapping = [("check",    return Check),
                           ("all in",   return AllIn),
                           ("raise",    getRaiseAmount game)]

foldCallRaiseAllIn :: Game -> IO (Action Int)
foldCallRaiseAllIn game = getAction actionMapping inputFoldCallRaiseAllIn 
                                    badFoldCallRaiseAllInInput game
    where actionMapping = [("fold",     return Fold),
                           ("call",     return Call),
                           ("all in",   return AllIn),
                           ("raise",    getRaiseAmount game)]

foldAllIn :: Game -> IO (Action Int)
foldAllIn = getAction actionMapping inputFoldAllIn badFoldAllInInput
    where actionMapping = [("fold",     return Fold),
                           ("all in",   return AllIn)]

foldCallAllIn :: Game -> IO (Action Int)
foldCallAllIn = getAction actionMapping inputFoldCallAllIn badFoldCallAllInInput
    where actionMapping = [("fold",     return Fold),
                           ("call",     return Call),
                           ("all in",   return AllIn)]

checkAllIn :: Game -> IO (Action Int)
checkAllIn = getAction actionMapping inputCheckAllIn badCheckAllInInput
    where actionMapping = [("check",    return Check),
                           ("all in",   return AllIn)]

getAction :: [(String, IO (Action Int))] -> String -> String -> Game 
                                         -> IO (Action Int)
getAction actionMapping inputMsg badInputMsg game = do
    printf inputMsg (player^.num+1) (player^.name) >> hFlush stdout
    input <- map toLower <$> getLine
    fromMaybe badInput (lookup input actionMapping)
    where badInput = do
            putStrLn badInputMsg
            getAction actionMapping inputMsg badInputMsg game
          player = getCurrentPlayer game

-- Note: raise amount is new bet value, not current bet + raise.
-- So, "I want to raise to 500" means if the current bet is 100, the new bet
-- will be 500, not 600.
getRaiseAmount :: Game -> IO (Action Int)
getRaiseAmount game = do
    printf inputRaise (player^.num+1) (player^.name) >> hFlush stdout
    input <- maybeRead <$> getLine
    case input of
        Nothing -> putStrLn raiseNotInteger >> getRaiseAmount game
        Just raise' -> if isValidRaise raise' game
                        then return $ Raise raise'
                        else putStrLn invalidRaiseAmount >> getRaiseAmount game
    where maybeRead = fmap fst . listToMaybe . reads
          player = getCurrentPlayer game

--raise' is the new bet, not the amount raised
isValidRaise :: Int -> Game -> Bool
isValidRaise raise' game = isAtLeastMinimumRaise && hasEnoughChips
    where isAtLeastMinimumRaise = raise' >= currentBet' + minRaise
          hasEnoughChips = chips' + bet' >= raise'
          minRaise = game^.bets.minimumRaise
          currentBet' = game^.bets.currentBet
          chips' = getCurrentPlayer game^.chips
          bet' = getCurrentPlayer game^.bet
