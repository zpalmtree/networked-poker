module ClientFramework
(
    establishConnection,
    ioLoop,
    ClientResponse(..),
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

data ClientResponse a = Nowt
                      | Something a
                      | GameWon
                      | GameLoss

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
                Left (_, _, err) -> error err
                Right (_, _, msg') -> case msg' of
                    MIsInitialGame (InitialGameMsg m) -> do
                        infoM "Prog.establishConnection" "Recieved initial game"

                        return $ Right (m, sock)

                    _ -> error "Invalid message recieved!"

ioLoop :: (Binary a, Binary b, Show b, MonadTrans c, Monad (c IO)) => 
           Socket -> (a -> c IO (ClientResponse b)) -> c IO Bool
ioLoop sock handleFunc = do
    msg <- lift $ recv sock 4096
    decode msg sock handleFunc

decode :: (Binary a, Binary b, Show b, MonadTrans c, Monad (c IO)) => 
           ByteString -> Socket -> (a -> c IO (ClientResponse b)) -> c IO Bool
decode input sock handleFunc = case decodeOrFail $ fromStrict input of
    Left (_, _, err) -> error err
    Right (unconsumed, _, msg) -> do
        maybeAction <- handleFunc msg

        let proceed = if BL.null unconsumed
                        then ioLoop sock handleFunc
                        else decode (toStrict unconsumed) sock handleFunc

        case maybeAction of
            GameWon -> return True
            GameLoss -> return False
            Nowt -> proceed
            Something action -> do
                lift . infoM "Prog.decode" $ 
                    "Sending message to server: " ++ show action
                
                lift . send sock . toStrict $ encode action

                proceed
