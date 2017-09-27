module Utilities.Types
(
    fromPure
)
where

import Control.Monad.Trans.State (StateT(..), runState)

import Types (GameState, GameStateT)

fromPure :: GameState a -> GameStateT a
fromPure f = StateT (return . runState f)
