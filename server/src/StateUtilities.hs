module StateUtilities where

import Types

import Control.Lens

isShowdown :: Game -> Bool
isShowdown game = case game^.state of
    Showdown -> True
    _ -> False
