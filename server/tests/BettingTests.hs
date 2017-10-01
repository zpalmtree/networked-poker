{-# LANGUAGE TemplateHaskell #-}

module BettingTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Control.Lens ((^..), traversed)
import Control.Monad.Trans.State (execState)

import Types (Game)
import Lenses (bet, chips, bets, pots, pot, playerQueue, players)
import Betting (updatePot)

prop_updatePotChipsPreserved :: Game -> Bool
prop_updatePotChipsPreserved initialState = beforeChips == afterChips
    where s = execState updatePot initialState
          beforeChips = allChips initialState
          afterChips = allChips s

          allChips lens = sum $ map sum [lens^..allPlayers.bet,
                                         lens^..allPlayers.chips,
                                         lens^..bets.pots.traversed.pot]

          allPlayers = playerQueue.players.traversed

--this block needs to be at the bottom of the file apparently
return []
runTests :: IO Bool
runTests = $quickCheckAll
