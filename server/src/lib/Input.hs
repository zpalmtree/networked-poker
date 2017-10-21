module Input
(
    checkRaiseAllIn,
    checkAllIn,
    foldCallRaiseAllIn,
    foldAllIn,
    foldCallAllIn
)
where

import Text.Printf (printf)
import Control.Lens ((^.))
import Data.ByteString.Lazy (fromStrict)
import Network.Socket.ByteString (recv)
import Control.Monad.Trans.Class (lift)
import Data.Binary (decodeOrFail)
import System.Log.Logger (warningM, infoM)

import Utilities.Player (getCurrentPlayer)
import Lenses (socket)
import Types (GameStateT, Action(..))
import Output (outputInputRequest, outputBadInput)

getAction :: [Action Int] -> Action Int -> GameStateT (Action Int)
getAction actions def = do
    p <- getCurrentPlayer

    outputInputRequest actions

    lift $ infoM "Prog.getAction" "Waiting for a response from client"

    msg <- lift $ recv (p^.socket) 4096

    lift $ infoM "Prog.getAction" "Recieved response from client"

    case decodeOrFail $ fromStrict msg of
        Left (_, _, err) -> do
            lift . warningM "Prog.getAction" 
                 $ "Error decoding message: " ++ err ++ ", folding player."
            return def
        Right (_, _, msg') -> if msg' `elem` actions
            then do
                lift . infoM "Prog.getAction" $
                       printf "Recieved message %s from client\n" (show msg')

                return msg'

            else do
                outputBadInput

                lift . warningM "Prog.getAction" $ 
                       printf badInput (show actions) (show msg')

                return def
    where badInput = "Message recieved invalid, expected one of %s, got %s"

--have to supply a dummy parameter to raise
checkRaiseAllIn :: GameStateT (Action Int)
checkRaiseAllIn = getAction [Check, Raise 0, AllIn] Check

checkAllIn :: GameStateT (Action Int)
checkAllIn = getAction [Check, AllIn] Check

foldCallRaiseAllIn :: GameStateT (Action Int)
foldCallRaiseAllIn = getAction [Fold, Call, Raise 0, AllIn] Fold

foldAllIn :: GameStateT (Action Int)
foldAllIn = getAction [Fold, AllIn] Fold

foldCallAllIn :: GameStateT (Action Int)
foldCallAllIn = getAction [Fold, Call, AllIn] Fold
