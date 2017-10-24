module Setup
(
    initialSetup,
    initialGUISetup,
    makeClass
)
where

import Control.Concurrent.MVar (newEmptyMVar)
import Data.IORef (IORef, newIORef, readIORef)
import Control.Exception (IOException, try)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict)
import Data.Binary (encode)
import Control.Lens ((^.))
import Data.Text (pack)
import System.Log.Logger (infoM)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrSocketType, addrProtocol,
     connect, addrAddress)

import Graphics.QML 
    (Class, ObjRef, defPropertySigRO', newClass, newSignalKey, 
     defMethod')

import ClientTypes (StatesNSignals(..), CGame(..), CGameStateT)
import Types (Message(..))
import Utilities (getName, decode)
import Lenses (clientGame)
import Constants (maxPlayers, numTCards, numButtons, cardBack)
import HandleClick (handleFold, handleCheck, handleCall, handleRaise, handleAllIn)

import GUIUpdate 
    (updateNames, updateBets, updateCards, updateVisible, updateCurrentPlayer)

makeClass :: IO (Class (), StatesNSignals)
makeClass = do
    let cards = replicate 2 cardBack

    -- BUTTONS
    bEnabledSig <- newSignalKey
    bEnabledS <- newIORef $ replicate numButtons False

    -- TABLE CARDS
    tCardsSig <- newSignalKey
    tCardsS <- newIORef $ replicate numTCards cardBack

    -- PLAYER CARDS
    pCardsSig <- newSignalKey
    pCardsS <- newIORef $ replicate maxPlayers cards

    -- PLAYER CHIPS
    pChipsSig <- newSignalKey
    pChipsS <- newIORef $ replicate maxPlayers 0 :: IO (IORef [Int])

    -- POT CHIPS
    potChipsSig <- newSignalKey
    potChipsS <- newIORef (0 :: Int)

    -- PLAYER NAMES
    pNamesSig <- newSignalKey
    pNamesS <- newIORef names

    -- PLAYERS VISIBLE
    pVisibleSig <- newSignalKey
    pVisibleS <- newIORef $ replicate maxPlayers True

    -- PLAYERS INPLAY
    pInPlaySig <- newSignalKey
    pInPlayS <- newIORef $ replicate maxPlayers True

    --CURRENT PLAYER BORDER
    pCurrentPlayerSig <- newSignalKey
    pCurrentPlayerS <- newIORef $ replicate maxPlayers False

    --SLIDER VALUES
    slideMinSig <- newSignalKey
    slideMaxSig <- newSignalKey
    slideMinS <- newIORef 0
    slideMaxS <- newIORef 0

    --GAME OVER WINDOWS
    lossWindowVisibleSig <- newSignalKey
    winWindowVisibleSig <- newSignalKey
    lossWindowVisibleS <- newIORef False
    winWindowVisibleS <- newIORef False

    m <- newEmptyMVar

    let sNs = StatesNSignals pCardsSig              pCardsS
                             pChipsSig              pChipsS
                             pNamesSig              pNamesS
                             tCardsSig              tCardsS
                             bEnabledSig            bEnabledS
                             potChipsSig            potChipsS
                             pVisibleSig            pVisibleS
                             pInPlaySig             pInPlayS
                             pCurrentPlayerSig      pCurrentPlayerS
                             slideMinSig            slideMinS
                             slideMaxSig            slideMaxS
                             lossWindowVisibleSig   lossWindowVisibleS
                             winWindowVisibleSig    winWindowVisibleS
                             m

    rootClass <- newClass [
        defPropertySigRO' "bEnabled" bEnabledSig $ defRead bEnabledS,
        defPropertySigRO' "pVisible" pVisibleSig $ defRead pVisibleS,
        defPropertySigRO' "pInPlay" pInPlaySig $ defRead pInPlayS,
        defPropertySigRO' "pCurrentPlayer" pCurrentPlayerSig 
                        $ defRead pCurrentPlayerS,
        defPropertySigRO' "tCards" tCardsSig $ defRead tCardsS,
        defPropertySigRO' "pNames" pNamesSig $ defRead pNamesS,
        defPropertySigRO' "pCards" pCardsSig $ defRead pCardsS,
        defPropertySigRO' "potValue" potChipsSig $ defRead potChipsS,
        defPropertySigRO' "pBets" pChipsSig $ defRead pChipsS,
        defPropertySigRO' "slideMin" slideMinSig $ defRead slideMinS,
        defPropertySigRO' "slideMax" slideMaxSig $ defRead slideMaxS,
        defPropertySigRO' "lossWindowVisible" lossWindowVisibleSig 
                        $ defRead lossWindowVisibleS,
        defPropertySigRO' "winWindowVisible" winWindowVisibleSig
                        $ defRead winWindowVisibleS,

        defMethod' "fold" (handleFold sNs),
        defMethod' "check" (handleCheck sNs),
        defMethod' "call" (handleCall sNs),
        defMethod' "raiseN" (handleRaise sNs),
        defMethod' "allIn" (handleAllIn sNs)]

    return (rootClass, sNs)

    where defRead s _ = readIORef s
          names = replicate maxPlayers (pack "")

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
                        infoM "Prog.initialSetup" "Recieved initial game"

                        return $ Right (CGame (m^.clientGame) sigs this, sock)

                    _ -> error "Invalid message recieved!"

initialGUISetup :: CGameStateT ()
initialGUISetup = do
    updateVisible
    updateNames
    updateBets
    updateCards
    updateCurrentPlayer
