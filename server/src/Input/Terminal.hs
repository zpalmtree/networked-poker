module Input.Terminal
(
    checkRaiseAllIn,
    checkAllIn,
    foldCallRaiseAllIn,
    foldAllIn,
    foldCallAllIn
)
where

import Types
import Messages
import PlayerUtilities

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
    putStr inputMsg >> hFlush stdout
    input <- map toLower <$> getLine
    fromMaybe badInput (lookup input actionMapping)
    where badInput = do
            putStrLn badInputMsg
            getAction actionMapping inputMsg badInputMsg game

{- Note: raise amount is new bet value, not current bet + raise.
So, "I want to raise to 500" means if the current bet is 100, the new bet will
be 500, not 600. -}
getRaiseAmount :: Game -> IO (Action Int)
getRaiseAmount game = do
    putStr inputRaise >> hFlush stdout
    input <- maybeRead <$> getLine
    case input of
        Nothing -> putStrLn raiseNotInteger >> getRaiseAmount game
        Just raise' -> if isValidRaise raise' game
                        then return $ Raise raise'
                        else putStrLn invalidRaiseAmount >> getRaiseAmount game
    where maybeRead = fmap fst . listToMaybe . reads

isValidRaise :: Int -> Game -> Bool
isValidRaise raise' game = raiseSize >= game^.bets.minimumRaise
                        && getCurrentPlayer game^.chips >= raiseSize
    where raiseSize = raise' - game^.bets.currentBet
