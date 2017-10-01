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
import Data.Maybe (listToMaybe)
import System.IO (hFlush, stdout)
import Control.Lens ((^.))
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)

import Types (Game, Action(..), GameState, GameStateT)
import Output.Terminal.InputMessages
import Utilities.Types (fromPure)
import Lenses (bets, currentBet, uuid, name, bet, chips, minimumRaise)

import Utilities.Player 
    (getCurrentPlayer, getCurrentPlayerPure, getCurrentPlayerT)

--prelude pls go
foldMap' :: (String, GameStateT (Action a))
foldMap' = ("fold", return Fold)

checkMap :: (String, GameStateT (Action a))
checkMap = ("check", return Check)

callMap :: (String, GameStateT (Action a))
callMap = ("call", return Call)

allInMap :: (String, GameStateT (Action a))
allInMap = ("all in", return AllIn)

raiseMap :: (String, GameStateT (Action Int))
raiseMap = ("raise", getRaiseAmount)

checkRaiseAllIn :: GameStateT (Action Int)
checkRaiseAllIn = getAction aMap inputCheckRaiseAllIn badCheckRaiseAllInInput
    where aMap = [checkMap, allInMap, raiseMap]

foldCallRaiseAllIn :: GameStateT (Action Int)
foldCallRaiseAllIn = do
    s <- get
    
    let msg = inputFoldCallRaiseAllIn (s^.bets.currentBet)
    
    getAction aMap msg badFoldCallRaiseAllInInput
    where aMap = [foldMap', callMap, allInMap, raiseMap]

foldAllIn :: GameStateT (Action Int)
foldAllIn = getAction aMap inputFoldAllIn badFoldAllInInput
    where aMap = [foldMap', allInMap]

foldCallAllIn :: GameStateT (Action Int)
foldCallAllIn = do
    s <- get
    let msg = inputFoldCallAllIn (s^.bets.currentBet)
    
    getAction aMap msg badFoldCallAllInInput

    where aMap = [foldMap', callMap, allInMap]

checkAllIn :: GameStateT (Action Int)
checkAllIn = getAction aMap inputCheckAllIn badCheckAllInInput
    where aMap = [checkMap, allInMap]

getAndHandleInput :: String -> (String -> a) -> (a -> GameStateT b) -> 
                     GameStateT b
getAndHandleInput inputMsg transformF caseFunc = do
    player <- getCurrentPlayerT

    input <- lift $ do
        printf inputMsg (player^.name) (player^.bet) (player^.chips)

        hFlush stdout
        transformF <$> getLine

    caseFunc input

{-# ANN getAction "HLint: ignore Use fromMaybe" #-}
getAction :: [(String, GameStateT a)] -> String -> String -> GameStateT a
getAction actionMap inputMsg badInputMsg = 
    getAndHandleInput inputMsg (map toLower) caseFunc

    where caseFunc x = case lookup x actionMap of
                        Nothing -> do
                            lift $ putStrLn badInputMsg
                            getAction actionMap inputMsg badInputMsg
                        Just action -> action

-- Note: raise amount is new bet value, not current bet + raise.
-- So, "I want to raise to 500" means if the current bet is 100, the new bet
-- will be 500, not 600.
getRaiseAmount :: GameStateT (Action Int)
getRaiseAmount = getAndHandleInput inputRaise maybeRead caseFunc
    where caseFunc x = case x of
                        Nothing -> do
                            lift $ putStrLn raiseNotInteger
                            getRaiseAmount
                        Just raise -> handleRaise raise

          maybeRead = fmap fst . listToMaybe . reads

handleRaise :: Int -> GameStateT (Action Int)
handleRaise raise = do
    s <- get

    handleRaise' s raise

handleRaise' :: Game -> Int -> GameStateT (Action Int)
handleRaise' s raise
    | raise < minRaiseAbsolute = def lessThanMinimumRaise'
    | chips' + bet' < raise = def notEnoughChips'
    | otherwise = return $ Raise raise
    where def msgF = do
            msg <- fromPure msgF
            lift $ putStrLn msg
            getRaiseAmount

          minRaise = s^.bets.minimumRaise
          currentBet' = s^.bets.currentBet
          chips' = getCurrentPlayerPure s^.chips
          bet' = getCurrentPlayerPure s^.bet
          minRaiseAbsolute = currentBet' + minRaise

lessThanMinimumRaise' :: GameState String
lessThanMinimumRaise' = do
    s <- get

    let currentBet' = s^.bets.currentBet
        minRaise = s^.bets.minimumRaise

    return . printf lessThanMinimumRaise $ currentBet' + minRaise

notEnoughChips' :: GameState String
notEnoughChips' = do
    player <- getCurrentPlayer

    return . printf notEnoughChips $ player^.chips + player^.bet
