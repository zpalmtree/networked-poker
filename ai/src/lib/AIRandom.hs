module AIRandom
(
    handleFunc
)
where

import System.Random (randomR, getStdRandom)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))

import AITypes (AIGameStateT)
import Types (Action(..))
import Lenses (cIsMe, cPlayers, cBets, cMinimumRaise, cChips, cBet)

handleFunc :: [Action Int] -> AIGameStateT (Action Int)
handleFunc options = do
    index <- lift . getStdRandom $ randomR (0, length options - 1)

    case options !! index of
        Raise _ -> getRaise
        x -> return x

    where getRaise :: AIGameStateT (Action Int)
          getRaise = do
            s <- get

            let min' = s^.cBets.cMinimumRaise
                me = head $ filter (^.cIsMe) (s^.cPlayers)
                max' = me^.cChips + me^.cBet

            num <- lift . getStdRandom $ randomR (min', max')

            return $ Raise num
