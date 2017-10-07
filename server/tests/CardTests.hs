{-# LANGUAGE TemplateHaskell #-}

module CardTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pre)
import Test.QuickCheck.Property (Property, once)
import Control.Lens ((^..), (^.), traversed)
import Control.Monad.Trans.State (execStateT)

import Types (Game, Stage(..))
import Utilities.Card (dealCards, hearts, clubs, diamonds, spades, fullDeck)
import Lenses (playerQueue, players, deck, cardInfo, cards, stage)

prop_fullDeckValid :: Property
prop_fullDeckValid = once $ lengthCorrect && unique fullDeck
    where lengthCorrect = length fullDeck == 52
          unique [] = True
          unique (x:xs)
            | x `elem` xs = False
            | otherwise = unique xs

-- this only needs to ran once, no arguments passed
prop_suitsValid :: Property 
prop_suitsValid = once $ lengthsCorrect && allUnique
    where lengthsCorrect = all (\x -> length x == 13) suits
          allUnique = all unique suits
          suits = [hearts, clubs, diamonds, spades]
          unique [] = True
          unique (x:xs)
            | x `elem` xs = False
            | otherwise = unique xs

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
