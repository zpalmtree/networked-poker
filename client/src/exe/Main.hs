{-# LANGUAGE GADTs, RankNTypes #-}
module Main
(
    main
)
where

import Data.Binary (Binary, encode, decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict, fromStrict)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Control.Concurrent (forkIO)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (pack)
import Control.Exception (IOException, try)

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Graphics.QML
    (SignalKey, Class, initialDocument, contextObject, newClass, newObject,
     defaultEngineConfig, fileDocument, anyObjRef, newSignalKey,
     defPropertySigRO', runEngineLoop)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrProtocol, addrSocketType, 
     addrAddress, connect, withSocketsDo)

import Paths_client (getDataFileName)

import Lenses (clientGame)

import ClientTypes (StatesNSignals(..), CGame(..), CGameStateT)

import Types 
    (Message(..), ActionMsg, PlayerTurnMsg, CardMsg, DealtCardsMsg, 
     PotWinnersMsg, GameOverMsg, PlayersRemovedMsg, CardRevealMsg)

main :: IO ()
main = withSocketsDo $ do   
    gui <- getDataFileName "src/gui/Main.qml"
    
    (rootClass, sigs) <- makeClass

    ctx <- newObject rootClass ()

    trySetup <- initialSetup sigs

    case trySetup of
        (Left err) -> do
            putStrLn "Couldn't connect to server. Did you start it?"
            print err

        (Right (initialState, sock)) -> do

            forkIO $ evalStateT (ioLoop sock) initialState

            runEngineLoop defaultEngineConfig {
                initialDocument = fileDocument gui,
                contextObject = Just $ anyObjRef ctx
            }

--for some odd reason, eta reducing func prevents it from compiling
{-# ANN makeClass "HLint: ignore Eta reduce" #-}
makeClass :: IO (Class (), StatesNSignals)
makeClass = do
    cardBack <- pack <$> getDataFileName "src/gui/assets/card-back.png"
    let cards = replicate 2 cardBack

    -- BUTTONS
    bEnabledSig <- nsk
    bEnabledS <- newIORef $ replicate numButtons False

    -- TABLE CARDS
    tCardsSig <- nsk
    tCardsS <- newIORef $ replicate numCards cardBack

    -- PLAYER CARDS
    pCardsSig <- nsk
    pCardsS <- newIORef $ replicate numPlayers cards

    -- PLAYER CHIPS
    pChipsSig <- nsk
    pChipsS <- newIORef $ replicate numPlayers 1000 :: IO (IORef [Int])

    -- POT CHIPS
    potChipsSig <- nsk
    potChipsS <- newIORef (0 :: Int)

    -- PLAYER NAMES
    pNamesSig <- nsk
    pNamesS <- newIORef names

    -- PLAYERS VISIBLE
    pVisibleSig <- nsk
    pVisibleS <- newIORef $ replicate numPlayers True

    -- can't have polymorphic lists
    let boolL   = [("bEnabled",     bEnabledSig,    bEnabledS),
                   ("pVisible",     pVisibleSig,    pVisibleS)]

        text    = [("tCards",       tCardsSig,      tCardsS),

                   ("pNames",       pNamesSig,      pNamesS)]

        textL   = [("pCards",       pCardsSig,      pCardsS)]

        ints    = [("potValue",     potChipsSig,    potChipsS)]

        intsL   = [("pChips",       pChipsSig,      pChipsS)]

    let func xs = map (\(x, y, z) -> defPropertySigRO' x y $ defRead z) xs

        sNs = StatesNSignals pCardsSig      pCardsS
                             pChipsSig      pChipsS
                             pNamesSig      pNamesS
                             tCardsSig      tCardsS
                             bEnabledSig    bEnabledS
                             potChipsSig    potChipsS
                             pVisibleSig    pVisibleS

    rootClass <- newClass $ func boolL ++
                            func text ++ 
                            func textL ++ 
                            func ints ++ 
                            func intsL

    return (rootClass, sNs)

    where defRead s _ = readIORef s
          numPlayers = 6
          numCards = 5
          numButtons = 5
          names = map pack ["Bob", "Dave", "Steve", "Jim", "Pete", "Gary"]

          -- so I don't have to list the type every time
          nsk :: IO (SignalKey (IO ()))
          nsk = newSignalKey

initialSetup :: StatesNSignals -> IO (Either IOException (CGame, Socket))
initialSetup sigs = do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2112")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connectSuccess <- try (connect sock (addrAddress addr))

    case connectSuccess of
        Left err -> return $ Left err
        Right _ -> do

            name <- getName
            
            send sock (toStrict . encode $ name)

            msg <- recv sock 4096

            case decode msg of
                Left (_, _, err) -> error err
                Right (_, _, msg') -> case msg' of
                    MIsInitialGame m -> do
                        putStrLn "Recieved initial game..."
                        return $ Right (CGame (m^.clientGame) sigs, sock)
                    _ -> error "Invalid message recieved!"

getName :: IO String
getName = do
    putStr "Enter your name: "
    hFlush stdout
    getLine

ioLoop :: Socket -> CGameStateT ()
ioLoop sock = do
    msg <- lift $ decode <$> recv sock 4096
    case msg of
        Left (_, _, err) -> error err
        Right (_, _, msg') -> do
            handleMsg msg'
            ioLoop sock

decode :: (Binary a) => BS.ByteString -> 
                        Either (BL.ByteString, ByteOffset, String)
                               (BL.ByteString, ByteOffset, a)
decode msg = decodeOrFail $ fromStrict msg

handleMsg :: Message -> CGameStateT ()
handleMsg msg = case msg of
    MIsAction m -> handleAction m
    MIsPlayerTurn m -> handlePlayerTurn m
    MIsCard m -> handleNewCards m
    MIsDealt m -> handleMyCards m
    MIsPotWinners m -> handlePotWinners m
    MIsGameOver m -> handleGameOver m
    MIsPlayersRemoved m -> handlePlayersRemoved m
    MIsCardReveal m -> handleCardsRevealed m

handleAction :: ActionMsg a -> CGameStateT ()
handleAction = undefined

handlePlayerTurn :: PlayerTurnMsg -> CGameStateT ()
handlePlayerTurn = undefined

handleNewCards :: CardMsg -> CGameStateT ()
handleNewCards = undefined

handleMyCards :: DealtCardsMsg -> CGameStateT ()
handleMyCards = undefined

handlePotWinners :: PotWinnersMsg -> CGameStateT ()
handlePotWinners = undefined

handleGameOver :: GameOverMsg -> CGameStateT ()
handleGameOver = undefined

handlePlayersRemoved :: PlayersRemovedMsg -> CGameStateT ()
handlePlayersRemoved = undefined

handleCardsRevealed :: CardRevealMsg -> CGameStateT ()
handleCardsRevealed = undefined
