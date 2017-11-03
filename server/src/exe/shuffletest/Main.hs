module Main
(
    main
)
where

import Data.Map.Lazy (Map, insertWith, empty, toAscList)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T (empty)
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Control.Lens ((.~))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (openTempFile, hClose)

import Graphics.Rendering.Chart.Easy 
    (Renderable, layout_x_axis, laxis_generate, addIndexes, plotBars, def,
     plot_bars_values, toRenderable, layout_plots, layout_title, autoIndexAxis)

import Graphics.QML 
    (ObjRef, SignalKey, initialDocument, contextObject, newClass, defMethod',
     defaultEngineConfig, fileDocument, anyObjRef, runEngineLoop, newSignalKey,
     defPropertySigRO', newObject, fireSignal)

import DrawCard (dealHandKnuth, dealHandRandomIndex)
import Types (Card)

import Paths_server (getDataFileName)

data StatesNSignals = StatesNSignals {
    _chartLocationSig :: SignalKey (IO ()),
    _chartLocationS :: IORef Text
}

main :: IO ()
main = do
    chartLocationSig <- newSignalKey
    chartLocationS <- newIORef T.empty

    let sNs = StatesNSignals chartLocationSig chartLocationS

    rootClass <- newClass [
        defMethod' "testShuffle" (testShuffle sNs),
        defPropertySigRO' "chartLocation" chartLocationSig 
            $ defRead chartLocationS]
    
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
testShuffle sNs this shuffleType iterations = case unpack shuffleType of
    "RandomIndex" -> do
        mapping <- dealNHands iterations dealHandRandomIndex
        outputMapping sNs this mapping
    "KnuthShuffle" -> do
        mapping <- dealNHands iterations dealHandKnuth
        outputMapping sNs this mapping

outputMapping :: StatesNSignals -> ObjRef () -> Map Card Int -> IO ()
outputMapping sNs this mapping = do
    (fileName, handle) <- openTempFile "/tmp" "cardspicked" 
    hClose handle

    renderableToFile def fileName (chart mapping)

    writeIORef (_chartLocationS sNs) $ pack fileName
    fireSignal (_chartLocationSig sNs) this

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
