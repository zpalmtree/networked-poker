{-# LANGUAGE GADTs, TypeFamilies #-}

module Setup
(
    initialSetup,
    initialGUISetup,
    makeClass
)
where

import Data.IORef (IORef, newIORef, readIORef)
import Control.Exception (IOException, try)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict)
import Data.Binary (encode)
import Control.Lens ((^.))
import Data.Text (pack)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrSocketType, addrProtocol,
     connect, addrAddress)

import Graphics.QML 
    (Class, SignalKey, ObjRef, defPropertySigRO', newClass, newSignalKey)

import ClientTypes (StatesNSignals(..), CGame(..), CGameStateT)
import Types (Message(..))
import Utilities (getName, decode)
import Lenses (clientGame)
import GUIUpdate (updateNames, updateBets, updateCards, updateVisible)
import Constants (maxPlayers, numTCards, numButtons, cardBack)

--for some odd reason, eta reducing func prevents it from compiling
{-# ANN makeClass "HLint: ignore Eta reduce" #-}
makeClass :: IO (Class (), StatesNSignals)
makeClass = do
    let cards = replicate 2 cardBack

    -- BUTTONS
    bEnabledSig <- nsk
    bEnabledS <- newIORef $ replicate numButtons False

    -- TABLE CARDS
    tCardsSig <- nsk
    tCardsS <- newIORef $ replicate numTCards cardBack

    -- PLAYER CARDS
    pCardsSig <- nsk
    pCardsS <- newIORef $ replicate maxPlayers cards

    -- PLAYER CHIPS
    pChipsSig <- nsk
    pChipsS <- newIORef $ replicate maxPlayers 0 :: IO (IORef [Int])

    -- POT CHIPS
    potChipsSig <- nsk
    potChipsS <- newIORef (0 :: Int)

    -- PLAYER NAMES
    pNamesSig <- nsk
    pNamesS <- newIORef names

    -- PLAYERS VISIBLE
    pVisibleSig <- nsk
    pVisibleS <- newIORef $ replicate maxPlayers True

    -- PLAYERS INPLAY
    pInPlaySig <- nsk
    pInPlayS <- newIORef $ replicate maxPlayers True

    -- can't have polymorphic lists
    let boolL   = [("bEnabled",     bEnabledSig,    bEnabledS),
                   ("pVisible",     pVisibleSig,    pVisibleS),
                   ("pInPlay",      pInPlaySig,     pInPlayS)]

        text    = [("tCards",       tCardsSig,      tCardsS),

                   ("pNames",       pNamesSig,      pNamesS)]

        textL   = [("pCards",       pCardsSig,      pCardsS)]

        ints    = [("potValue",     potChipsSig,    potChipsS)]

        intsL   = [("pBets",        pChipsSig,      pChipsS)]

    let func xs = map (\(x, y, z) -> defPropertySigRO' x y $ defRead z) xs

        sNs = StatesNSignals pCardsSig      pCardsS
                             pChipsSig      pChipsS
                             pNamesSig      pNamesS
                             tCardsSig      tCardsS
                             bEnabledSig    bEnabledS
                             potChipsSig    potChipsS
                             pVisibleSig    pVisibleS
                             pInPlaySig     pInPlayS

    rootClass <- newClass $ func boolL ++
                            func text ++ 
                            func textL ++ 
                            func ints ++ 
                            func intsL

    return (rootClass, sNs)

    where defRead s _ = readIORef s
          names = replicate maxPlayers (pack "")

          -- so I don't have to list the type every time
          nsk :: IO (SignalKey (IO ()))
          nsk = newSignalKey

initialSetup :: StatesNSignals -> ObjRef () 
             -> IO (Either IOException (CGame, Socket))
initialSetup sigs this = do
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
                        return $ Right (CGame (m^.clientGame) sigs this, sock)
                    _ -> error "Invalid message recieved!"

initialGUISetup :: CGameStateT ()
initialGUISetup = do
    updateVisible
    updateNames
    updateBets
    updateCards
