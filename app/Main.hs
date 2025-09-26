{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Granite

main :: IO ()
main = do
    -- scatter
    let ptsA_x = [0..599]
    let ptsA_y = [0..599]
    let ptsB_x = [0..599]
    let ptsB_y = [599,598..0]
    Text.putStrLn $ scatter [series "A" (zip ptsA_x ptsA_y), series "B" (zip ptsB_x ptsB_y)] defPlot{widthChars = 68, heightChars = 22, plotTitle = "Random points"}

    -- histogram
    let heights = [100..200]
    Text.putStrLn $
        histogram
            (bins 30 155 195)
            heights
            defPlot
                { widthChars = 68
                , heightChars = 18
                , legendPos = LegendBottom
                , xFormatter = \_ _ v -> Text.pack (show (round v :: Int))
                , xNumTicks = 10
                , yNumTicks = 5
                , plotTitle = "Heights (cm)"
                }

    -- bars
    Text.putStrLn $ bars [("Q1", 12), ("Q2", 18), ("Q3", 9), ("Q4", 15)] defPlot{plotTitle = "Sales", xNumTicks = 4}

    -- pie
    Text.putStrLn $
        pie
            [("Alpha", 0.35), ("Beta", 0.25), ("Gamma", 0.20), ("Delta", 0.20)]
            defPlot{widthChars = 46, heightChars = 18, legendPos = LegendRight, plotTitle = "Share"}

    -- line graph
    Text.putStrLn $
        lineGraph
            [ ("Product A", [(1, 100), (2, 120), (3, 115), (4, 140), (5, 155), (6, 148)])
            , ("Product B", [(1, 80), (2, 85), (3, 95), (4, 92), (5, 110), (6, 125)])
            , ("Product C", [(1, 60), (2, 62), (3, 70), (4, 85), (5, 82), (6, 90)])
            ]
            defPlot{plotTitle = "Monthly Sales Trends"}

    -- box plot
    Text.putStrLn $
        boxPlot
            [ ("Class A", [78, 82, 85, 88, 90, 92, 85, 87, 89, 91, 76, 94, 88])
            , ("Class B", [70, 75, 72, 80, 85, 78, 82, 77, 79, 81, 74, 83])
            , ("Class C", [88, 92, 95, 90, 93, 89, 91, 94, 96, 87, 90, 92])
            , ("Class D", [65, 70, 72, 68, 75, 80, 73, 71, 69, 74, 77, 76])
            ]
            defPlot{plotTitle = "Test Score Distribution by Class"}

    -- stacked bar chart
    Text.putStrLn $
        stackedBars
            [ ("Q1", [("Hardware", 120), ("Software", 200), ("Services", 80)])
            , ("Q2", [("Hardware", 135), ("Software", 220), ("Services", 95)])
            , ("Q3", [("Hardware", 110), ("Software", 240), ("Services", 110)])
            , ("Q4", [("Hardware", 145), ("Software", 260), ("Services", 125)])
            ]
            defPlot{plotTitle = "Quarterly Revenue Breakdown", xNumTicks = 4}

    -- heatmap
    let matrix =
            [ [1.0, 0.8, 0.3, -0.2, 0.1]
            , [0.8, 1.0, 0.5, -0.1, 0.2]
            , [0.3, 0.5, 1.0, 0.6, 0.4]
            , [-0.2, -0.1, 0.6, 1.0, 0.7]
            , [0.1, 0.2, 0.4, 0.7, 1.0]
            ]
    Text.putStrLn $ heatmap matrix defPlot{plotTitle = "Correlation Matrix"}
