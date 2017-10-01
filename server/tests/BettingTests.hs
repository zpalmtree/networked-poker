{-# LANGUAGE TemplateHaskell #-}

module BettingTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Property (Property, (==>))
import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execState)

import Types (Game)
import Lenses (bet, chips, bets, pots, pot, playerQueue, players)
import Betting (updatePot)

prop_giveWinningsChipsPreserved :: Int -> Game -> Property
prop_giveWinningsChipsPreserved n s 
    = valid ==> chipsPreserved s
    where valid = n < length (s^.playerQueue.players)

chipsPreserved :: Game -> Bool
chipsPreserved s = beforeChips == afterChips
    where s' = execState updatePot s
          beforeChips = allChips s
          afterChips = allChips s'

          allChips lens = sum $ map sum [lens^..allPlayers.bet,
                                         lens^..allPlayers.chips,
                                         lens^..bets.pots.traversed.pot]

          allPlayers = playerQueue.players.traversed

prop_updatePotRunTwice :: Game -> Bool
prop_updatePotRunTwice s = once == twice
    where once = execState updatePot s
          twice = execState updatePot once

prop_updatePotChipsPreserved :: Game -> Bool
prop_updatePotChipsPreserved = chipsPreserved

--this block needs to be at the bottom of the file apparently
return []
runTests :: IO Bool
runTests = $quickCheckAll
