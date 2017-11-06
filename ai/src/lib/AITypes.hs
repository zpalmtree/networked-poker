module AITypes
(
    AIGameStateT,
    AIGameState
)
where

import Control.Monad.Trans.State (StateT)

import Types (ClientGame)

type AIGameStateT a = StateT ClientGame IO a
type AIGameState m a = StateT ClientGame m a
