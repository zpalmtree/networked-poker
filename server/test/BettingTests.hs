{-# LANGUAGE TemplateHaskell #-}

module BettingTests
(
    runTests
)
where

import Test.QuickCheck.All (quickCheckAll)

import Betting ()

--this block needs to be at the bottom of the file apparently
return []
runTests :: IO Bool
runTests = $quickCheckAll
