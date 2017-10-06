{-# LANGUAGE TemplateHaskell #-}

module CardTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pre)
import Test.QuickCheck.Property (Property)
import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execStateT)

import Types (Game, Stage(..))
import Utilities.Card (dealCards)
import Lenses (playerQueue, players, deck, cardInfo, cards, stage)

prop_dealCardsCardsPreserved :: Game -> Property
prop_dealCardsCardsPreserved s = monadicIO $ do
    --players haven't been dealt yet
    pre $ (s^.stage) == PreFlop
    s' <- run $ execStateT dealCards s

    assert (length (allCards s) == length (allCards s'))
    where allCards s' = concat $ s'^..playerQueue.players.traversed.cards
                     ++ s'^..cardInfo.deck

return []
runTests :: IO Bool
runTests = $quickCheckAll
