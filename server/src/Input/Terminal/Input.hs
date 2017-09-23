module Input.Terminal.Input
(
    checkRaiseAllIn,
    checkAllIn,
    foldCallRaiseAllIn,
    foldAllIn,
    foldCallAllIn
)
where

import Text.Printf (printf)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO (hFlush, stdout)
import Control.Lens hiding (Fold)

import Types (Game, Action(..))
import Output.Terminal.InputMessages
import PlayerUtilities (getCurrentPlayer)
import Lenses (bets, currentBet, num, name, bet, chips, minimumRaise)

checkRaiseAllIn :: Game -> IO (Action Int)
checkRaiseAllIn game = getAction actionMapping inputCheckRaiseAllIn
                                 badCheckRaiseAllInInput game
    where actionMapping = [("check",    return Check),
                           ("all in",   return AllIn),
                           ("raise",    getRaiseAmount game)]

foldCallRaiseAllIn :: Game -> IO (Action Int)
foldCallRaiseAllIn game = getAction actionMapping msg
                                    badFoldCallRaiseAllInInput game
    where actionMapping = [("fold",     return Fold),
                           ("call",     return Call),
                           ("all in",   return AllIn),
                           ("raise",    getRaiseAmount game)]
          msg = inputFoldCallRaiseAllIn (game^.bets.currentBet)

foldAllIn :: Game -> IO (Action Int)
foldAllIn = getAction actionMapping inputFoldAllIn badFoldAllInInput
    where actionMapping = [("fold",     return Fold),
                           ("all in",   return AllIn)]

foldCallAllIn :: Game -> IO (Action Int)
foldCallAllIn game = getAction actionMapping msg badFoldCallAllInInput game
    where actionMapping = [("fold",     return Fold),
                           ("call",     return Call),
                           ("all in",   return AllIn)]
          msg = inputFoldCallAllIn (game^.bets.currentBet)

checkAllIn :: Game -> IO (Action Int)
checkAllIn = getAction actionMapping inputCheckAllIn badCheckAllInInput
    where actionMapping = [("check",    return Check),
                           ("all in",   return AllIn)]

getAction :: [(String, IO (Action Int))] -> String -> String -> Game 
                                         -> IO (Action Int)
getAction actionMapping inputMsg badInputMsg game = do
    printf inputMsg (player^.num+1) (player^.name) (player^.bet) (player^.chips)
    hFlush stdout
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
    printf inputRaise (player^.num+1) (player^.name) (player^.bet) 
                      (player^.chips)
    hFlush stdout
    input <- maybeRead <$> getLine
    case input of
        Nothing -> putStrLn raiseNotInteger >> getRaiseAmount game
        Just raise -> handleRaise raise game
    where maybeRead = fmap fst . listToMaybe . reads
          player = getCurrentPlayer game

handleRaise :: Int -> Game -> IO (Action Int)
handleRaise raise game
    | raise < minRaiseAbsolute = putStrLn (lessThanMinimumRaise' game) >> def
    | chips' + bet' < raise = putStrLn (notEnoughChips' game) >> def
    | otherwise = return $ Raise raise
    where minRaise = game^.bets.minimumRaise
          currentBet' = game^.bets.currentBet
          chips' = getCurrentPlayer game^.chips
          bet' = getCurrentPlayer game^.bet
          minRaiseAbsolute = currentBet' + minRaise
          def = getRaiseAmount game

lessThanMinimumRaise' :: Game -> String
lessThanMinimumRaise' game = printf lessThanMinimumRaise minRaiseAbsolute
    where minRaiseAbsolute = currentBet' + minRaise
          currentBet' = game^.bets.currentBet
          minRaise = game^.bets.minimumRaise

notEnoughChips' :: Game -> String
notEnoughChips' game = printf notEnoughChips maxBet
    where chips' = getCurrentPlayer game^.chips
          bet' = getCurrentPlayer game^.bet
          maxBet = chips' + bet'
