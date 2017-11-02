module Main
(
    main
)
where

import Data.Map.Lazy (Map, insertWith, empty, toAscList)
import Data.Text (Text, unpack)
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Control.Monad (void)
import Control.Lens ((.~))

import Graphics.Rendering.Chart.Easy 
    (Renderable, layout_x_axis, laxis_generate, addIndexes, plotBars, def,
     plot_bars_values, toRenderable, layout_plots, layout_title, autoIndexAxis)

import Graphics.QML 
    (ObjRef, initialDocument, contextObject, newClass, defMethod', newObject,
     defaultEngineConfig, fileDocument, anyObjRef, runEngineLoop)

import DrawCard (dealHandKnuth, dealHandRandomIndex)
import Types (Card)

import Paths_server (getDataFileName)


main :: IO ()
main = do
    rootClass <- newClass [defMethod' "testShuffle" testShuffle]
    
    ctx <- newObject rootClass ()

    gui <- getDataFileName "src/gui/ShuffleTester.qml"

    let config = defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    runEngineLoop config

dealNHands :: Int -> IO [Card] -> IO (Map Card Int)
dealNHands n f = dealNHands' n f empty

dealNHands' :: Int -> IO [Card] -> Map Card Int -> IO (Map Card Int)
dealNHands' 0 _ acc = return acc
dealNHands' n f acc = do
    hand <- f
    dealNHands' (n-1) f $ updateAccumulator acc hand

updateAccumulator :: Map Card Int -> [Card] -> Map Card Int
updateAccumulator = foldl (\acc x -> insertWith (+) x 1 acc)

testShuffle :: ObjRef () -> Text -> Int -> IO ()
testShuffle _ shuffleType iterations = case unpack shuffleType of
    "RandomIndex" -> do
        mapping <- dealNHands iterations dealHandRandomIndex
        outputMapping mapping
    "KnuthShuffle" -> do
        mapping <- dealNHands iterations dealHandKnuth
        outputMapping mapping

outputMapping :: Map Card Int -> IO ()
outputMapping mapping' = void $ 
    renderableToFile def "example.png" (chart mapping')

{-# ANN chart "HLint: ignore Use ." #-}
chart :: Map Card Int -> Renderable ()
chart mapping' = toRenderable layout
    where barChart = plot_bars_values .~ addIndexes y_axis_values
                   $ def
    
          layout = layout_title .~ "Number of cards drawn"
                 $ layout_x_axis . laxis_generate .~ autoIndexAxis x_axis_labels
                 $ layout_plots .~ [plotBars barChart]
                 $ def
        
          list = toAscList mapping'
          x_axis_labels = map (show . fst) list
          y_axis_values = map (\x -> [snd x]) list
