{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import Granite

main :: IO ()
main = do
    section "Scatter — two series, 600 points each"
    Text.IO.putStrLn $
        let ptsA = [(x, x) | x <- [0 .. 599]]
            ptsB = [(x, 599 - x) | x <- [0 .. 599]]
         in scatter
                [series "A" ptsA, series "B" ptsB]
                defPlot{widthChars = 68, heightChars = 22, plotTitle = "Random points"}

    section "Line graph — three product trends"
    Text.IO.putStrLn $
        lineGraph
            [ ("Product A", [(1, 100), (2, 120), (3, 115), (4, 140), (5, 155), (6, 148)])
            , ("Product B", [(1, 80), (2, 85), (3, 95), (4, 92), (5, 110), (6, 125)])
            , ("Product C", [(1, 60), (2, 62), (3, 70), (4, 85), (5, 82), (6, 90)])
            ]
            defPlot{plotTitle = "Monthly Sales Trends"}

    section "Bar chart — quarterly sales"
    Text.IO.putStrLn $
        bars
            [("Q1", 12), ("Q2", 18), ("Q3", 9), ("Q4", 15)]
            defPlot{plotTitle = "Sales", xNumTicks = 4}

    section "Stacked bar chart — revenue by product type"
    Text.IO.putStrLn $
        stackedBars
            [ ("Q1", [("Hardware", 120), ("Software", 200), ("Services", 80)])
            , ("Q2", [("Hardware", 135), ("Software", 220), ("Services", 95)])
            , ("Q3", [("Hardware", 110), ("Software", 240), ("Services", 110)])
            , ("Q4", [("Hardware", 145), ("Software", 260), ("Services", 125)])
            ]
            defPlot{plotTitle = "Quarterly Revenue Breakdown"}

    section "Histogram — sample heights"
    let heights = concat $ zipWith replicate [0 .. 40] [160 .. 200]
    Text.IO.putStrLn $
        histogram
            (bins 40 155 195)
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

    section "Pie chart — market share"
    Text.IO.putStrLn $
        pie
            [("Alpha", 0.35), ("Beta", 0.25), ("Gamma", 0.20), ("Delta", 0.20)]
            defPlot
                { widthChars = 46
                , heightChars = 18
                , legendPos = LegendRight
                , plotTitle = "Share"
                }

    section "Heatmap — correlation matrix"
    let matrix =
            [ [1.0, 0.8, 0.3, -0.2, 0.1]
            , [0.8, 1.0, 0.5, -0.1, 0.2]
            , [0.3, 0.5, 1.0, 0.6, 0.4]
            , [-0.2, -0.1, 0.6, 1.0, 0.7]
            , [0.1, 0.2, 0.4, 0.7, 1.0]
            ]
    Text.IO.putStrLn $ heatmap matrix defPlot{plotTitle = "Correlation Matrix"}

    section "Box plot — test score distribution by class"
    Text.IO.putStrLn $
        boxPlot
            [ ("Class A", [78, 82, 85, 88, 90, 92, 85, 87, 89, 91, 76, 94, 88])
            , ("Class B", [70, 75, 72, 80, 85, 78, 82, 77, 79, 81, 74, 83])
            , ("Class C", [88, 92, 95, 90, 93, 89, 91, 94, 96, 87, 90, 92])
            , ("Class D", [65, 70, 72, 68, 75, 80, 73, 71, 69, 74, 77, 76])
            ]
            defPlot{plotTitle = "Test Score Distribution by Class"}

    section "Area — filled curve from y=0"
    Text.IO.putStrLn $
        area
            [ ("revenue", [(x, sin (x / 2) * 50 + 200) | x <- [0 .. 12 :: Double]])
            ]
            defPlot{widthChars = 60, heightChars = 18, plotTitle = "Daily revenue"}

    section "Ribbon — confidence band"
    Text.IO.putStrLn $
        ribbon
            [ ("CI 90%", [(x, sin x - 0.4, sin x + 0.4) | x <- [0, 0.5 .. 6.0 :: Double]])
            ]
            defPlot{widthChars = 60, heightChars = 16, plotTitle = "sin(x) +/- 0.4 band"}

    section "Density — KDE over a sample"
    Text.IO.putStrLn $
        density
            [
                ( "class A"
                , concat [replicate 5 v | v <- [1.0, 1.4, 1.6, 2.0, 2.3, 2.6, 3.0, 3.2, 3.5]]
                )
            ,
                ( "class B"
                , concat [replicate 5 v | v <- [2.5, 2.7, 3.0, 3.4, 3.8, 4.1, 4.4, 4.7, 5.0]]
                )
            ]
            defPlot{widthChars = 60, heightChars = 16, plotTitle = "Distribution of scores"}

    section "Error bars — points with vertical CIs"
    Text.IO.putStrLn $
        errorBars
            [
                ( "treatment"
                ,
                    [ (1, 2.1, 1.5, 2.7)
                    , (2, 3.5, 2.8, 4.2)
                    , (3, 5.2, 4.6, 5.8)
                    , (4, 4.7, 4.0, 5.4)
                    , (5, 6.1, 5.3, 6.9)
                    ]
                )
            ]
            defPlot{widthChars = 60, heightChars = 16, plotTitle = "Measurements +/- CI"}

    section "Funnel — sales pipeline"
    Text.IO.putStrLn $
        funnel
            [ ("Visited", 1000)
            , ("Signed up", 720)
            , ("Confirmed", 480)
            , ("Active", 220)
            , ("Paid", 120)
            ]
            defPlot{widthChars = 60, heightChars = 16, plotTitle = "Sales funnel"}

    section "Polar line — four-petal rose r = |sin(2θ)|"
    let nSteps = 32 :: Int
        roseTheta i = fromIntegral i / fromIntegral nSteps * 2 * pi
        rosePts = [(roseTheta i, abs (sin (2 * roseTheta i))) | i <- [0 .. nSteps]]
    Text.IO.putStrLn $
        polarLine
            [("r = |sin(2theta)|", rosePts)]
            defPlot{widthChars = 40, heightChars = 22, plotTitle = "Polar rose"}

    section "Waterfall — running revenue total"
    Text.IO.putStrLn $
        waterfall
            [ ("Start", 0, 100)
            , ("Q1", 100, 130)
            , ("Q2", 130, 180)
            , ("Q3", 180, 175) -- a small dip
            , ("Refund", 175, 195)
            , ("Net", 0, 195)
            ]
            defPlot{widthChars = 60, heightChars = 16, plotTitle = "Revenue waterfall"}

    section "Distplot — histogram + density overlay"
    Text.IO.putStrLn $
        distPlot
            [
                ( "sample"
                , concat
                    [ replicate 5 v
                    | v <-
                        [ 1.0
                        , 1.2
                        , 1.3
                        , 1.5
                        , 1.7
                        , 2.0
                        , 2.1
                        , 2.3
                        , 2.5
                        , 2.8
                        , 3.0
                        , 3.1
                        , 3.3
                        , 3.7
                        , 3.9
                        , 4.0
                        , 4.2
                        ]
                    ]
                )
            ]
            defPlot{widthChars = 60, heightChars = 18, plotTitle = "Distribution"}

section :: Text -> IO ()
section name = do
    Text.IO.putStrLn ""
    Text.IO.putStrLn (Text.replicate 72 "=")
    Text.IO.putStrLn name
    Text.IO.putStrLn (Text.replicate 72 "-")
    Text.IO.putStrLn ""
