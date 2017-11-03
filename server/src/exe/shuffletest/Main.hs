{-# LANGUAGE TemplateHaskell #-}

module Main
(
    main
)
where

import Data.Map.Lazy (Map, insertWith, empty, toAscList)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T (empty)
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Control.Lens ((.~), (^.), makeLenses)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (openTempFile, hClose)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)

import Graphics.Rendering.Chart.Easy 
    (Renderable, layout_x_axis, laxis_generate, addIndexes, plot_bars_values, 
     toRenderable, layout_plots, layout_title, autoIndexAxis, def, plotBars)

import Graphics.QML 
    (ObjRef, SignalKey, initialDocument, contextObject, newClass, defMethod',
     defaultEngineConfig, fileDocument, anyObjRef, runEngineLoop, newSignalKey,
     defPropertySigRO', newObject, fireSignal)

import DrawCard (dealHandKnuth, dealHandRandomIndex)
import Types (Card)

import Paths_server (getDataFileName)

data StatesNSignals = StatesNSignals {
    _chartLocationSig :: SignalKey (IO ()),
    _chartLocationS :: IORef Text,
    _guiEnabledSig :: SignalKey (IO ()),
    _guiEnabledS :: IORef Bool
}

makeLenses ''StatesNSignals

main :: IO ()
main = do
    chartLocationSig' <- newSignalKey
    chartLocationS' <- newIORef T.empty

    guiEnabledSig' <- newSignalKey
    guiEnabledS' <- newIORef True

    let sNs = StatesNSignals chartLocationSig'  chartLocationS'
                             guiEnabledSig'     guiEnabledS'

    rootClass <- newClass [
        defMethod' "testShuffle" (testShuffle sNs),
        defPropertySigRO' "guiEnabled" guiEnabledSig'
            $ defRead guiEnabledS',
        defPropertySigRO' "chartLocation" chartLocationSig'
            $ defRead chartLocationS']
    
    ctx <- newObject rootClass ()

    gui <- getDataFileName "src/gui/ShuffleTester.qml"

    let config = defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    runEngineLoop config

    where defRead s _ = readIORef s

dealNHands :: Int -> IO [Card] -> IO (Map Card Int)
dealNHands n f = dealNHands' n f empty

dealNHands' :: Int -> IO [Card] -> Map Card Int -> IO (Map Card Int)
dealNHands' 0 _ acc = return acc
dealNHands' n f acc = do
    hand <- f
    dealNHands' (n-1) f $ updateAccumulator acc hand

updateAccumulator :: Map Card Int -> [Card] -> Map Card Int
updateAccumulator = foldl (\acc x -> insertWith (+) x 1 acc)

testShuffle :: StatesNSignals -> ObjRef () -> Text -> Int -> IO ()
testShuffle sNs this shuffleType iterations = void . forkIO $ do
    (fileName, handle) <- openTempFile "/tmp" "cardspicked" 
    hClose handle

    flipGUI sNs this

    when (iterations >= 10000) $ do
        pending <- getDataFileName "src/gui/assets/loading.png"

        setImg sNs this pending

    let handF = case unpack shuffleType of
            "RandomIndex" -> dealHandRandomIndex
            "KnuthShuffle" -> dealHandKnuth
            _ -> error "Unexpected shuffle type!"

    mapping <- dealNHands iterations handF

    renderableToFile def fileName (chart mapping)

    setImg sNs this fileName

    flipGUI sNs this

flipGUI :: StatesNSignals -> ObjRef () -> IO ()
flipGUI sNs this = do
    enabled <- readIORef (sNs^.guiEnabledS)

    writeIORef (sNs^.guiEnabledS) (not enabled)
    fireSignal (sNs^.guiEnabledSig) this

setImg :: StatesNSignals -> ObjRef () -> String -> IO ()
setImg sNs this loc = do
    writeIORef (sNs^.chartLocationS) $ pack loc
    fireSignal (sNs^.chartLocationSig) this

{-# ANN chart "HLint: ignore Use ." #-}
chart :: Map Card Int -> Renderable ()
chart mapping = toRenderable layout
    where barChart = plot_bars_values .~ addIndexes y_axis_values
                   $ def
    
          layout = layout_title .~ "Number of cards drawn"
                 $ layout_x_axis . laxis_generate .~ autoIndexAxis x_axis_labels
                 $ layout_plots .~ [plotBars barChart]
                 $ def
        
          list = toAscList mapping
          x_axis_labels = map (show . fst) list
          y_axis_values = map (\x -> [snd x]) list
