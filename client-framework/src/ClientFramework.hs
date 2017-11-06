module ClientFramework
(
    establishConnection,
    ioLoop
)
where

import Control.Exception (IOException, try)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Network.Socket.ByteString (send, recv)
import Data.Binary (Binary, encode, decodeOrFail)
import System.Log.Logger (infoM)
import Control.Monad (void)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (null)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrSocketType, addrProtocol,
     connect, addrAddress, withSocketsDo)

import Types (Message(..), InitialGameMsg(..), ClientGame)

establishConnection :: String -> IO (Either String (ClientGame, Socket))
establishConnection name = withSocketsDo $ do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connectSuccess <- try $ connect sock (addrAddress addr) 
                   :: IO (Either IOException ())

    case connectSuccess of
        Left err -> return . Left $ show err
        Right _ -> do
            send sock (toStrict . encode $ name)

            msg <- recv sock 4096

            case decodeOrFail $ fromStrict msg of
                Left (_, _, err) -> return $ Left err
                Right (_, _, msg') -> case msg' of
                    MIsInitialGame (InitialGameMsg m) -> do
                        infoM "Prog.establishConnection" "Recieved initial game"

                        return $ Right (m, sock)

                    _ -> error "Invalid message recieved!"

ioLoop :: (Binary a, Binary b, Show b, MonadTrans c, Monad (c IO)) => 
           Socket -> (a -> c IO (Maybe b)) -> c IO ()
ioLoop sock handleFunc = do
    msg <- lift $ recv sock 4096
    decode msg sock handleFunc

decode :: (Binary a, Binary b, Show b, MonadTrans c, Monad (c IO)) => 
           ByteString -> Socket -> (a -> c IO (Maybe b)) -> c IO ()
decode input sock handleFunc = case decodeOrFail $ fromStrict input of
    Left (_, _, err) -> error err
    Right (unconsumed, _, msg) -> do
        maybeAction <- handleFunc msg
        case maybeAction of
            Nothing -> return ()
            Just action -> do
                lift . infoM "Prog.decode" $ 
                    "Sending message to server: " ++ show action
                
                void . lift . send sock . toStrict $ encode action

        if BL.null unconsumed
            then ioLoop sock handleFunc
            else decode (toStrict unconsumed) sock handleFunc
