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
import Data.ByteString.Lazy (toStrict, fromStrict)
import Network.Socket.ByteString (send, recv)
import Control.Monad.Trans.Class (lift)
import Data.Binary (encode, decodeOrFail)

import Utilities.Player (getCurrentPlayerT)
import Lenses (socket)
import Types (GameStateT, Action(..), InputMsg(..), BadInputMsg(..))

getAction :: [Action Int] -> Action Int -> GameStateT (Action Int)
getAction actions def = do
    p <- getCurrentPlayerT
    
    let sock = p^.socket

    lift $ send sock (toStrict . encode $ InputMsg actions)
    msg <- lift $ recv sock 4096
    case (decodeOrFail $ fromStrict msg) of
        Left (_, _, err) -> do
            lift . putStrLn $ "Error decoding message: " ++ err
            return def
        Right (_, _, msg') -> if (msg' `elem` actions)
            then return msg'
            else do
                lift $ send (p^.socket) (toStrict $ encode BadInputMsg)
                lift . putStrLn $ printf badInput (show actions) (show msg')
                return def
    where badInput = "Message recieved invalid, expected one of %s, got %s"

--have to supply a dummy parameter to raise
checkRaiseAllIn :: GameStateT (Action Int)
checkRaiseAllIn = getAction [Check, Raise undefined, AllIn] Check

checkAllIn :: GameStateT (Action Int)
checkAllIn = getAction [Check, AllIn] Check

foldCallRaiseAllIn :: GameStateT (Action Int)
foldCallRaiseAllIn = getAction [Fold, Call, Raise undefined, AllIn] Fold

foldAllIn :: GameStateT (Action Int)
foldAllIn = getAction [Fold, AllIn] Fold

foldCallAllIn :: GameStateT (Action Int)
foldCallAllIn = getAction [Fold, Call, AllIn] Fold
