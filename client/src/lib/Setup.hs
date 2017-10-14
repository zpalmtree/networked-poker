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
import Data.Text (Text, pack)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Lazy (toStrict)
import Data.Binary (encode)
import Control.Lens ((^.), (^..), traversed)

import Network.Socket 
    (Socket, getAddrInfo, socket, addrFamily, addrSocketType, addrProtocol,
     connect, addrAddress)

import Graphics.QML 
    (Class, SignalKey, defPropertySigRO', newClass, newSignalKey)

import ClientTypes (StatesNSignals(..), CGame(..))
import Types (Message(..), Card)
import Utilities (getName, decode)
import Lenses (clientGame, cPlayerQueue, cPlayers, cCards, cChips, cName)
import CLenses (game)

import Paths_client (getDataFileName, getDataDir)

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
    tCardsS <- newIORef $ replicate numTCards cardBack

    -- PLAYER CARDS
    pCardsSig <- nsk
    pCardsS <- newIORef $ replicate maxPlayers cards

    -- PLAYER CHIPS
    pChipsSig <- nsk
    pChipsS <- newIORef $ replicate maxPlayers 1000 :: IO (IORef [Int])

    -- POT CHIPS
    potChipsSig <- nsk
    potChipsS <- newIORef (0 :: Int)

    -- PLAYER NAMES
    pNamesSig <- nsk
    pNamesS <- newIORef names

    -- PLAYERS VISIBLE
    pVisibleSig <- nsk
    pVisibleS <- newIORef $ replicate maxPlayers True

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
          names = replicate maxPlayers (pack "")

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

initialGUISetup :: CGame -> IO ()
initialGUISetup cgame = do
    dir <- getDataDir

    -- can't seem to use allPlayers on names and chips - it thinks it's only
    -- pointing to one thing - the cards?
    let names = cgame^..game.cPlayerQueue.cPlayers.traversed.cName
        chips = cgame^..game.cPlayerQueue.cPlayers.traversed.cChips
        cards = map (convertCards dir) $ cgame^..allPlayers.cCards
        allPlayers = game.cPlayerQueue.cPlayers.traversed
        visible = getVisible (length $ cgame^.game.cPlayerQueue.cPlayers)

    updateVisible cgame visible
    updateNames cgame (map pack names)
    updateChips cgame chips
    updateCards cgame cards

getVisible :: Int -> [Bool]
getVisible n = replicate n True ++ replicate (maxPlayers - n) False

maxPlayers :: Int
maxPlayers = 6

numTCards :: Int
numTCards = 5

numButtons :: Int
numButtons = 5

convertCards :: String -> [Card] -> [Text]
convertCards dir cs = case cs of
    [] -> map pack [cardBack, cardBack]
    [a, b] -> map pack [show a, show b]
    _ -> error "Invalid cards passed to convertCards!"
    where cardBack = cardDir ++ "card-back.png"
          cardDir = dir ++ "/src/gui/assets/"

updateNames :: CGame -> [Text] -> IO ()
updateNames = undefined

updateChips :: CGame -> [Int] -> IO ()
updateChips = undefined

updateCards :: CGame -> [[Text]] -> IO ()
updateCards = undefined

updateVisible :: CGame -> [Bool] -> IO ()
updateVisible = undefined
