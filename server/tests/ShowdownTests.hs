{-# LANGUAGE TemplateHaskell #-}

module ShowdownTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Property (Property, (==>))
import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execState)

import Types (Game, Stage(..))
import Showdown (distributePot, calculateHandValues)
import Lenses (pot, bet, chips, bets, pots, playerQueue, players, stage)

prop_calculateHandValues :: Game -> Property
prop_calculateHandValues s
    = valid ==> once == once

    where valid = s^.stage `elem` [River, Showdown]
          -- need to have all table cards to calculate hand values
          once = execState calculateHandValues s
                
prop_distributePotChipsPreserved :: Game -> Property
prop_distributePotChipsPreserved s 
    = valid ==> test

    where start = allChips s
          end = allChips $ execState f s
          pot' = head $ s^.bets.pots
          test = start + pot'^.pot == end
          f = do
            calculateHandValues
            distributePot pot'
          valid = s^.stage `elem` [River, Showdown]

allChips :: Game -> Int
allChips lens = sum $ map sum [lens^..allPlayers.bet,
                               lens^..allPlayers.chips,
                               lens^..bets.pots.traversed.pot]
    where allPlayers = playerQueue.players.traversed

--this block needs to be at the bottom of the file apparently
return []
runTests :: IO Bool
runTests = $quickCheckAll
