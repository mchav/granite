{-# LANGUAGE OverloadedStrings #-}

{- |
Regenerate @docs/tutorial.md@ from @docs/base/tutorial.md@ by
rendering each @\<!-- chart: SLUG -->@ marker to an SVG under
@docs/images/@ and inserting the image link after the code block.
-}
module Main where

import Control.Monad (forM_)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

baseFile, outputFile, imagesDir :: FilePath
baseFile = "docs/base/tutorial.md"
outputFile = "docs/tutorial.md"
imagesDir = "docs/images"

main :: IO ()
main = do
    createDirectoryIfMissing True imagesDir
    src <- Text.IO.readFile baseFile
    let rendered = renderTutorial src
    Text.IO.writeFile outputFile rendered
    forM_ fixtures $ \(slug, chart) -> do
        let svgPath = imagesDir </> Text.unpack slug <> ".svg"
        Text.IO.writeFile svgPath (renderChartSvg chart)
    putStrLn $ "Wrote " <> outputFile
    putStrLn $ "Wrote " <> show (length fixtures) <> " SVGs under " <> imagesDir

renderTutorial :: Text -> Text
renderTutorial src = Text.unlines (go (Text.lines src))
  where
    go :: [Text] -> [Text]
    go [] = []
    go (line : rest) = case chartSlug line of
        Just slug ->
            let (block, afterBlock) = takeCodeBlock rest
                output = renderChartOutput slug
             in line : block <> output <> go afterBlock
        Nothing -> line : go rest

chartSlug :: Text -> Maybe Text
chartSlug ln =
    let stripped = Text.strip ln
     in case Text.stripPrefix "<!-- chart:" stripped of
            Just rest -> case Text.stripSuffix "-->" (Text.strip rest) of
                Just slug -> Just (Text.strip slug)
                Nothing -> Nothing
            Nothing -> Nothing

takeCodeBlock :: [Text] -> ([Text], [Text])
takeCodeBlock = goOpen []
  where
    goOpen acc [] = (reverse acc, [])
    goOpen acc (ln : rest)
        | Text.isPrefixOf "```" (Text.stripStart ln) =
            goClose (ln : acc) rest
        | otherwise = goOpen (ln : acc) rest

    goClose acc [] = (reverse acc, [])
    goClose acc (ln : rest)
        | Text.isPrefixOf "```" (Text.stripStart ln) =
            (reverse (ln : acc), rest)
        | otherwise = goClose (ln : acc) rest

renderChartOutput :: Text -> [Text]
renderChartOutput slug = case lookup slug fixtures of
    Nothing ->
        [ ""
        , "> _(no fixture for chart slug `" <> slug <> "` — skipped)_"
        ]
    Just _chart ->
        let imgRel = "images/" <> slug <> ".svg"
         in [ ""
            , "![" <> slug <> "](" <> imgRel <> ")"
            ]

fixtures :: [(Text, Chart)]
fixtures =
    [ ("scatter", scatterChart)
    , ("line", lineChart)
    , ("bar", barChart)
    , ("bar-horizontal", barHorizontal)
    , ("bar-stacked", barStacked)
    , ("bar-grouped", barGrouped)
    , ("pie", pieChart)
    , ("area", areaChart)
    , ("histogram", histChart)
    , ("boxplot", boxChart)
    , ("errorbar", errorbarChart)
    , ("density", densityChart)
    , ("distplot", distplotChart)
    , ("heatmap", heatmapChart)
    , ("heatmap-annotated", heatmapAnnotated)
    , ("funnel", funnelChart)
    , ("waterfall", waterfallChart)
    , ("polar", polarChart)
    , ("facet", facetChart)
    , ("log-y", logYChart)
    , ("layer-bar-line", barLineChart)
    , ("layer-scatter-radius", scatterRadiusChart)
    , ("layer-scatter-fit", scatterFitChart)
    , ("layer-line-ribbon", lineRibbonChart)
    ]

scatterChart :: Chart
scatterChart =
    let xs = [0, 1, 2, 3, 4] :: [Double]
        ys = [0, 1, 4, 9, 16] :: [Double]
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        layer =
            (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "y = x^2"
            , chartSize = SizeChars 40 14
            }

lineChart :: Chart
lineChart =
    let xs = [0, 0.5 .. 6.0] :: [Double]
        ys = map sin xs
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        layer =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "sin(x)"
            , chartSize = SizeChars 50 14
            }

barChart :: Chart
barChart =
    let xs = ["A", "B", "C", "D"]
        ys = [12, 7, 18, 9] :: [Double]
        df = fromColumns [("x", ColCat xs), ("y", ColNum ys)]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Bars"
            , chartSize = SizeChars 40 14
            }

barHorizontal :: Chart
barHorizontal =
    let xs = ["Alpha", "Bravo", "Charlie", "Delta", "Echo"]
        ys = [82, 67, 45, 33, 21] :: [Double]
        df = fromColumns [("item", ColCat xs), ("value", ColNum ys)]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "item")
                        , aesY = Just (ColumnRef "value")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartCoord = CoordFlip
            , chartTitle = Just "Top 5 (horizontal)"
            , chartSize = SizeChars 60 16
            }

barStackedFrame :: DataFrame
barStackedFrame =
    let quarters = concat [replicate 3 q | q <- ["Q1", "Q2", "Q3", "Q4"]]
        products = take 12 (cycle ["Widgets", "Gadgets", "Gizmos"])
        sales = [12, 8, 4, 15, 10, 6, 18, 12, 8, 22, 14, 10] :: [Double]
     in fromColumns
            [ ("quarter", ColCat quarters)
            , ("product", ColCat products)
            , ("sales", ColNum sales)
            ]

barStacked :: Chart
barStacked =
    let layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "quarter")
                        , aesY = Just (ColumnRef "sales")
                        , aesGroup = Just (ColumnRef "product")
                        , aesFill = Just (ColumnRef "product")
                        }
                , layerStat = StatIdentity
                , layerPosition = PosStack
                }
     in emptyChart
            { chartData = barStackedFrame
            , chartLayers = [layer]
            , chartTitle = Just "Sales by quarter (stacked)"
            , chartSize = SizeChars 60 16
            }

barGrouped :: Chart
barGrouped =
    barStacked
        { chartLayers =
            [ (List.head (chartLayers barStacked))
                { layerPosition = PosDodge 0.25
                }
            ]
        , chartTitle = Just "Sales by quarter (grouped)"
        }

pieChart :: Chart
pieChart =
    let df =
            fromColumns
                [ ("slice", ColCat ["A", "B", "C", "D"])
                , ("value", ColNum [40, 25, 20, 15])
                ]
        layer =
            (defLayer GeomArc)
                { layerMapping = emptyMapping{aesY = Just (ColumnRef "value")}
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Pie"
            , chartSize = SizeChars 30 16
            }

areaChart :: Chart
areaChart =
    let xs = [0 .. 12] :: [Double]
        ymax = [sin (x / 2) + 2 | x <- xs]
        zeros = replicate (length xs) 0 :: [Double]
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("ymin", ColNum zeros)
                , ("ymax", ColNum ymax)
                ]
        layer =
            (defLayer GeomRibbon)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesYmin = Just (ColumnRef "ymin")
                        , aesYmax = Just (ColumnRef "ymax")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Filled area"
            , chartSize = SizeChars 56 14
            }

histChart :: Chart
histChart =
    let raw = take 50 (cycle [1.0, 1.2, 1.8, 2.0, 2.3, 2.7, 3.1, 3.4, 3.8, 4.2])
        df = fromColumns [("x", ColNum raw)]
        layer =
            (defLayer GeomHistogram)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "count")
                        }
                , layerStat = StatBin (BinByCount 8)
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Histogram"
            , chartSize = SizeChars 44 14
            }

boxChart :: Chart
boxChart =
    let pts =
            [("A", v) | v <- [1, 2, 3, 4, 5, 6, 7, 8, 9 :: Double]]
                <> [("B", v) | v <- [3, 4, 5, 6, 7, 8, 9, 10, 11]]
                <> [("C", v) | v <- [5, 6, 7, 8, 9, 10, 11, 12, 13]]
        df =
            fromColumns
                [ ("g", ColCat [g | (g, _) <- pts])
                , ("y", ColNum [v | (_, v) <- pts])
                ]
        layer =
            (defLayer GeomBoxplot)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "g")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerStat = StatBoxplot
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Boxplot"
            , chartSize = SizeChars 40 16
            }

errorbarChart :: Chart
errorbarChart =
    let xs = [1 .. 5] :: [Double]
        ys = [2.1, 3.5, 5.2, 4.7, 6.1] :: [Double]
        los = [1.5, 2.8, 4.6, 4.0, 5.3] :: [Double]
        his = [2.7, 4.2, 5.8, 5.4, 6.9] :: [Double]
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("y", ColNum ys)
                , ("lo", ColNum los)
                , ("hi", ColNum his)
                ]
        m =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                , aesYmin = Just (ColumnRef "lo")
                , aesYmax = Just (ColumnRef "hi")
                }
        bars = (defLayer GeomErrorbar){layerMapping = m, layerStat = StatIdentity}
        pts = (defLayer GeomPoint){layerMapping = m, layerStat = StatIdentity}
     in emptyChart
            { chartData = df
            , chartLayers = [bars, pts]
            , chartTitle = Just "Measurements +/- CI"
            , chartSize = SizeChars 56 14
            }

densitySample :: [Double]
densitySample =
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
    ]

densityChart :: Chart
densityChart =
    let df = fromColumns [("x", ColNum densitySample)]
        layer =
            (defLayer GeomDensity)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "density")
                        }
                , layerStat = StatDensity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Density (KDE)"
            , chartSize = SizeChars 56 14
            }

distplotChart :: Chart
distplotChart =
    let df = fromColumns [("x", ColNum densitySample)]
        hist =
            (defLayer GeomHistogram)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "count")
                        }
                , layerStat = StatBin (BinByCount 8)
                }
        kde =
            (defLayer GeomDensity)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "density")
                        }
                , layerStat = StatDensity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [hist, kde]
            , chartTitle = Just "Distribution"
            , chartSize = SizeChars 56 16
            }

heatmapCoords :: [(Double, Double, Double)]
heatmapCoords =
    [ ( fromIntegral x
      , fromIntegral y
      , sin (fromIntegral x / 2) + cos (fromIntegral y / 2)
      )
    | x <- [0 .. 4 :: Int]
    , y <- [0 .. 4 :: Int]
    ]

heatmapChart :: Chart
heatmapChart =
    let df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- heatmapCoords])
                , ("y", ColNum [y | (_, y, _) <- heatmapCoords])
                , ("z", ColNum [z | (_, _, z) <- heatmapCoords])
                ]
        layer =
            (defLayer GeomTile)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesFill = Just (ColumnRef "z")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Heatmap"
            , chartSize = SizeChars 48 16
            }

heatmapAnnotated :: Chart
heatmapAnnotated =
    let labels = [Text.pack (show (round (z * 10) :: Int)) | (_, _, z) <- heatmapCoords]
        df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- heatmapCoords])
                , ("y", ColNum [y | (_, y, _) <- heatmapCoords])
                , ("z", ColNum [z | (_, _, z) <- heatmapCoords])
                , ("label", ColCat labels)
                ]
        tile =
            (defLayer GeomTile)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesFill = Just (ColumnRef "z")
                        }
                , layerStat = StatIdentity
                }
        label =
            (defLayer GeomText)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesLabel = Just (ColumnRef "label")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [tile, label]
            , chartTitle = Just "Annotated heatmap"
            , chartSize = SizeChars 48 16
            }

funnelChart :: Chart
funnelChart =
    let xs = ["Visited", "Signed up", "Confirmed", "Active", "Paid"]
        ys = [1000, 720, 480, 220, 120] :: [Double]
        df = fromColumns [("stage", ColCat xs), ("count", ColNum ys)]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "stage")
                        , aesY = Just (ColumnRef "count")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartCoord = CoordFlip
            , chartTitle = Just "Sales funnel"
            , chartSize = SizeChars 60 14
            }

waterfallChart :: Chart
waterfallChart =
    let xs = ["Start", "Q1", "Q2", "Q3", "Refund", "Net"]
        ystart = [0, 100, 130, 180, 175, 0] :: [Double]
        yend = [100, 130, 180, 175, 195, 195] :: [Double]
        df =
            fromColumns
                [ ("x", ColCat xs)
                , ("y", ColNum yend)
                , ("__ybase", ColNum ystart)
                ]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Waterfall"
            , chartSize = SizeChars 60 14
            }

polarChart :: Chart
polarChart =
    let nSteps = 32 :: Int
        pts =
            [ (theta, abs (sin (2 * theta)))
            | i <- [0 .. nSteps]
            , let theta = (fromIntegral i / fromIntegral nSteps) * 2 * pi
            ]
        df =
            fromColumns
                [ ("theta", ColNum [t | (t, _) <- pts])
                , ("r", ColNum [r | (_, r) <- pts])
                ]
        layer =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "theta")
                        , aesY = Just (ColumnRef "r")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartCoord = CoordPolar ThetaX 0 PolarCCW
            , chartTitle = Just "r = |sin(2 theta)|"
            , chartSize = SizeChars 40 20
            }

facetChart :: Chart
facetChart =
    let df =
            fromColumns
                [ ("x", ColNum [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3])
                , ("y", ColNum [1, 4, 9, 16, 0, 2, 4, 6, 5, 4, 3, 2])
                , ("series", ColCat (replicate 4 "A" <> replicate 4 "B" <> replicate 4 "C"))
                ]
        layer =
            (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartFacet = FacetWrap (ColumnRef "series") (Just 3) Nothing ScalesFixed
            , chartTitle = Just "Faceted by series"
            , chartSize = SizeChars 70 18
            }

logYChart :: Chart
logYChart =
    let xs = [1 .. 6] :: [Double]
        ys = [3, 30, 80, 200, 700, 2100] :: [Double]
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        layer =
            (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartScales = defScales{scaleY = SLog Base10 defScaleOpts}
            , chartTitle = Just "Log Y"
            , chartSize = SizeChars 50 14
            }

barLineChart :: Chart
barLineChart =
    let months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun"]
        sales = [12, 18, 15, 24, 20, 28] :: [Double]
        movavg = [12, 15, 16.5, 19.5, 22, 24] :: [Double]
        df =
            fromColumns
                [ ("month", ColCat months)
                , ("sales", ColNum sales)
                , ("avg", ColNum movavg)
                ]
        bars =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "month")
                        , aesY = Just (ColumnRef "sales")
                        }
                , layerStat = StatIdentity
                }
        trend =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "month")
                        , aesY = Just (ColumnRef "avg")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightRed)
                        , defLineWidth = Just 2.5
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [bars, trend]
            , chartTitle = Just "Monthly sales + trend"
            , chartSize = SizeChars 60 14
            }

scatterRadiusChart :: Chart
scatterRadiusChart =
    let pts =
            [ (0.2, 0.5)
            , (0.8, 1.2)
            , (-0.5, 0.3)
            , (1.1, -0.2)
            , (1.8, 0.7)
            , (-0.3, -0.6)
            , (2.2, 1.5)
            , (1.5, -1.1)
            , (0.1, 2.1)
            , (2.5, 0.3)
            , (-1.0, 1.0)
            , (0.7, -0.4)
            ] ::
                [(Double, Double)]
        points =
            fromColumns
                [ ("lon", ColNum [x | (x, _) <- pts])
                , ("lat", ColNum [y | (_, y) <- pts])
                ]
        poi =
            fromColumns
                [ ("lon", ColNum [0.5])
                , ("lat", ColNum [0.5])
                ]
        radius =
            (defLayer GeomPoint)
                { layerData = Just poi
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "lon")
                        , aesY = Just (ColumnRef "lat")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightCyan)
                        , defSize = Just 60
                        , defAlpha = Just 0.25
                        }
                }
        scatter =
            (defLayer GeomPoint)
                { layerData = Just points
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "lon")
                        , aesY = Just (ColumnRef "lat")
                        }
                }
     in emptyChart
            { chartLayers = [radius, scatter]
            , chartTitle = Just "Points near POI"
            , chartSize = SizeChars 50 18
            }

scatterFitChart :: Chart
scatterFitChart =
    let xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
        noise = [0.3, -0.5, 0.7, -0.2, 0.4, -0.6, 0.1, 0.3, -0.4, 0.5]
        ys = [2 * x + 1 + n | (x, n) <- zip xs noise]
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        m =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        points = (defLayer GeomPoint){layerMapping = m}
        fit =
            (defLayer GeomLine)
                { layerMapping = m
                , layerStat = StatSmooth SmoothLm
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightRed)
                        , defLineWidth = Just 2
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [points, fit]
            , chartTitle = Just "Scatter + OLS fit"
            , chartSize = SizeChars 50 14
            }

lineRibbonChart :: Chart
lineRibbonChart =
    let xs = [0, 0.5 .. 6.0] :: [Double]
        ys = map sin xs
        los = map (\x -> sin x - 0.3) xs
        his = map (\x -> sin x + 0.3) xs
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("y", ColNum ys)
                , ("lo", ColNum los)
                , ("hi", ColNum his)
                ]
        ribbon =
            (defLayer GeomRibbon)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesYmin = Just (ColumnRef "lo")
                        , aesYmax = Just (ColumnRef "hi")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightCyan)
                        , defAlpha = Just 0.3
                        }
                }
        line =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightBlue)
                        , defLineWidth = Just 2
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [ribbon, line]
            , chartTitle = Just "sin(x) +/- 0.3 band"
            , chartSize = SizeChars 56 14
            }
