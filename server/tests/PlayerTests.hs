{-# LANGUAGE TemplateHaskell #-}

module PlayerTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Control.Lens ((^.), (^..), traversed)
import Control.Monad.Trans.State (runState)

import Types (Game)
import Utilities.Player (removeOutPlayers)
import Lenses (playerQueue, players, chips)

prop_removeOutPlayers :: Game -> Bool
prop_removeOutPlayers s = case maybePlayers of
    Nothing -> len s == len s'
    Just n -> (len s == len s' + length n) &&
              (sum (n^..traversed.chips) == 0)
    where (maybePlayers, s') = runState removeOutPlayers s
          len lens = length $ lens^.playerQueue.players

return []
runTests :: IO Bool
runTests = $quickCheckAll
