module AIRuleBased
(
    handleFunc
)
where

import Data.Poker (numericalHandValue, fromList)
import Data.Poker.Internal (unNumericalHandValue)
import Control.Monad.Trans.State (get)
import Control.Lens ((^.))
import Safe (at)

import CoerceCard (cardToEval)
import AITypes (AIGameStateT)
import Types (Action(..), Card(..), ClientGame, CPlayer)

import Lenses 
    (cIsMe, cPlayers, cBets, cMinimumRaise, cCards, cBigBlindSize, cChips,
     cCurrentBet, cCommunityCards, cBet)

{-# ANN handleFunc "HLint: ignore Use head" #-}
handleFunc :: [Action Int] -> AIGameStateT (Action Int)
handleFunc options = do
    s <- get

    let me = head $ filter (^.cIsMe) (s^.cPlayers)
        myCards = me^.cCards
        evAllCards = evaluateCards $ myCards `at` 0 : myCards `at` 1 : s^.cCommunityCards
        evCommunityCards = evaluateCards $ s^.cCommunityCards
        ev = evAllCards - evCommunityCards

    return $ handleOptions options ev s me

handleOptions :: [Action Int] -> Int -> ClientGame -> CPlayer -> Action Int
handleOptions options ev s me
    -- crap hand, check if possible
    | targetBet <= 0 && Check `elem` options = Check
    -- can't check, so fold
    | targetBet <= 0 = Fold
    -- can make a valid raise, so do so
    | targetBet >= s^.cBets.cMinimumRaise + s^.cBets.cCurrentBet &&
      enoughChips = Raise targetBet
    -- can't make a valid raise but have a good hand, go all in
    -- pair of tens
    | ev >= 17301504 = AllIn
    -- can't make a valid raise but good hand, so call/check
    | targetBet >= s^.cBets.cCurrentBet && Check `elem` options = Check
    -- as above
    | targetBet >= s^.cBets.cCurrentBet && enoughChips = Call
    -- target bet isn't better than current, so check if possible
    | Check `elem` options = Check
    -- otherwise fold
    | otherwise = Fold
    where targetBet = targetBet' ev (s^.cBets.cBigBlindSize)
          enoughChips = me^.cBet + me^.cChips >= targetBet
    
-- minimum value is 65535
-- maximum value is 135004160
-- 0 EV is 786432
evaluateCards :: [Card] -> Int
evaluateCards = fromIntegral . unNumericalHandValue . numericalHandValue
              . fromList . map cardToEval

-- normalise to the average 0 EV value
targetBet' :: Int -> Int -> Int
targetBet' x bigBlindSize = round $ (fromIntegral x - 786432) * 
                                    fromIntegral bigBlindSize * 0.0000005
