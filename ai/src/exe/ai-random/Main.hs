module Main
(
    main
)
where

import System.Random (randomR, getStdRandom)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))

import AIFramework (runAI)
import AITypes (AIGameStateT)
import Types (Action(..))
import Lenses (cIsMe, cPlayers, cBets, cMinimumRaise, cChips, cBet)

main :: IO ()
main = runAI "ai-random" handleFunc

handleFunc :: [Action Int] -> AIGameStateT (Maybe (Action Int))
handleFunc options = do
    index <- lift . getStdRandom $ randomR (0, length options - 1)

    case options !! index of
        Raise _ -> (Just . Raise) <$> getRaise
        x -> return $ Just x

    where getRaise :: AIGameStateT Int
          getRaise = do
            s <- get

            let min' = s^.cBets.cMinimumRaise
                me = head $ filter (^.cIsMe) (s^.cPlayers)
                max' = me^.cChips + me^.cBet

            lift . getStdRandom $ randomR (min', max')
