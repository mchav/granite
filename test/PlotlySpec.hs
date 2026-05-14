{-# LANGUAGE OverloadedStrings #-}

{- |
Replicas of canonical examples from the Plotly basic / statistical /
scientific / financial galleries:

  * https://plotly.com/python/basic-charts/
  * https://plotly.com/python/statistical-charts/
  * https://plotly.com/python/scientific-charts/
  * https://plotly.com/python/financial-charts/

Each fixture is one chart, locked as a terminal + SVG golden. Charts
already covered by 'GoldenSpec' (line, scatter, vertical bar, pie,
histogram, box plot, polar rose, log-Y) are not duplicated here.

Out of scope (see memory @granite-ir-rewrite@ for the rationale):
3D, ternary, contour, network, dendrogram, sunburst, sankey, treemap,
gantt, candlestick / OHLC, gauge / indicator, imshow, wind rose,
violin, 2D-density heatmap.
-}
module PlotlySpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text

import Golden (goldenText)
import Granite.Data.Frame (Column (..), fromColumns)
import Granite.Render.Pipeline (renderChartSvg, renderChartTerminal)
import Granite.Spec (
    BinSpec (..),
    Chart (..),
    ColumnRef (..),
    Coord (..),
    Geom (..),
    Layer (..),
    Mapping (..),
    Position (..),
    Size (..),
    Stat (..),
    defLayer,
    emptyChart,
    emptyMapping,
 )
import Test.Hspec

------------------------------------------------------------------------
-- Spec entry
------------------------------------------------------------------------

spec :: Spec
spec = describe "Plotly gallery replicas (Phase 8)" $ do
    describe "Basic" $ do
        chartCase "plotly-bar-horizontal" horizontalBar
        chartCase "plotly-bar-stacked" stackedBar
        chartCase "plotly-bar-grouped" groupedBar
        chartCase "plotly-area-filled" filledArea

    describe "Statistical" $ do
        chartCase "plotly-error-bars" errorBars
        chartCase "plotly-density" densityCurve
        chartCase "plotly-distplot" distPlot

    describe "Scientific" $ do
        chartCase "plotly-heatmap" heatmap
        chartCase "plotly-heatmap-annotated" annotatedHeatmap

    describe "Financial" $ do
        chartCase "plotly-funnel" funnel
        chartCase "plotly-waterfall" waterfall

-- | Bless one chart as a (terminal, svg) pair of goldens.
chartCase :: String -> Chart -> SpecWith ()
chartCase name chart = do
    it (name <> " — terminal") $
        goldenText (name <> ".txt") (renderChartTerminal chart)
    it (name <> " — svg") $
        goldenText (name <> ".svg") (renderChartSvg chart)

------------------------------------------------------------------------
-- Basic
------------------------------------------------------------------------

{- | Horizontal bar chart: five items ranked left-to-right (highest
value at top). Built from a vertical bar + 'CoordFlip'.
-}
horizontalBar :: Chart
horizontalBar =
    let xs = [0, 1, 2, 3, 4] :: [Double]
        ys = [82, 67, 45, 33, 21] :: [Double]
        df = fromColumns [("item", ColNum xs), ("value", ColNum ys)]
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

{- | Stacked bar chart: four quarters × three product lines. Long
format + 'PosStack' splits each X-column into a vertical stack.
-}
stackedBar :: Chart
stackedBar =
    let quarters = concat [replicate 3 q | q <- [0, 1, 2, 3 :: Double]]
        products = take 12 (cycle ["Product A", "Product B", "Product C"])
        sales = [12, 8, 4, 15, 10, 6, 18, 12, 8, 22, 14, 10] :: [Double]
        df =
            fromColumns
                [ ("quarter", ColNum quarters)
                , ("product", ColCat products)
                , ("sales", ColNum sales)
                ]
        layer =
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
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Sales by quarter (stacked)"
            , chartSize = SizeChars 60 16
            }

{- | Grouped (dodged) bar chart: same data as 'stackedBar' but each
product sits side-by-side at every quarter via 'PosDodge'.
-}
groupedBar :: Chart
groupedBar =
    stackedBar
        { chartLayers =
            [ (head (chartLayers stackedBar))
                { layerPosition = PosDodge 0.25
                }
            ]
        , chartTitle = Just "Sales by quarter (grouped)"
        }

{- | Filled area chart: a smooth curve with the region between the
curve and 'aesYmin'=0 shaded.
-}
filledArea :: Chart
filledArea =
    let xs = [0 .. 12] :: [Double]
        ys = [sin (x / 2) + 2 | x <- xs]
        zeros = replicate (length xs) 0 :: [Double]
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("ymin", ColNum zeros)
                , ("ymax", ColNum ys)
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

------------------------------------------------------------------------
-- Statistical
------------------------------------------------------------------------

{- | Five (x, y) points each with a vertical confidence interval shown
as an 'MPolyline' error bar. Two layers: GeomPoint over GeomErrorbar.
-}
errorBars :: Chart
errorBars =
    let xs = [1, 2, 3, 4, 5] :: [Double]
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
        pts = (defLayer GeomPoint){layerMapping = m, layerStat = StatIdentity}
        bars = (defLayer GeomErrorbar){layerMapping = m, layerStat = StatIdentity}
     in emptyChart
            { chartData = df
            , chartLayers = [bars, pts]
            , chartTitle = Just "Measurements ± CI"
            , chartSize = SizeChars 56 14
            }

{- | Density (KDE) curve over a small sample. StatDensity produces a
(grid x, density) frame; GeomDensity renders it as a line.
-}
densityCurve :: Chart
densityCurve =
    let sample =
            [ 1.0
            , 1.2
            , 1.3
            , 1.4
            , 1.5
            , 1.7
            , 2.0
            , 2.1
            , 2.2
            , 2.3
            , 2.5
            , 2.8
            , 3.0
            , 3.1
            , 3.3
            , 3.4
            , 3.7
            , 3.9
            , 4.0
            , 4.2
            ] ::
                [Double]
        df = fromColumns [("x", ColNum sample)]
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

{- | Two layers on shared axes: a histogram of the sample with a KDE
line on top. The layers share the chart-level data frame.
-}
distPlot :: Chart
distPlot =
    let sample =
            [ 1.0
            , 1.2
            , 1.3
            , 1.4
            , 1.5
            , 1.7
            , 2.0
            , 2.1
            , 2.2
            , 2.3
            , 2.5
            , 2.8
            , 3.0
            , 3.1
            , 3.3
            , 3.4
            , 3.7
            , 3.9
            , 4.0
            , 4.2
            ] ::
                [Double]
        df = fromColumns [("x", ColNum sample)]
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

------------------------------------------------------------------------
-- Scientific
------------------------------------------------------------------------

{- | A 5×5 heatmap with intensity varying as @sin(x) + cos(y)@. Rendered
via 'GeomTile' — the SVG backend gets distinct fill rectangles per
cell; terminal renders the cell outlines.
-}
heatmap :: Chart
heatmap =
    let coords =
            [ ( fromIntegral x
              , fromIntegral y
              , sin (fromIntegral x / 2) + cos (fromIntegral y / 2)
              )
            | x <- [0 .. 4 :: Int]
            , y <- [0 .. 4 :: Int]
            ]
        df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- coords])
                , ("y", ColNum [y | (_, y, _) <- coords])
                , ("z", ColNum [z | (_, _, z) <- coords])
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

{- | Same data as 'heatmap' but with value labels overlaid in each
cell via a 'GeomText' layer. Plotly\'s "annotated heatmap" example.
-}
annotatedHeatmap :: Chart
annotatedHeatmap =
    let coords =
            [ ( fromIntegral x
              , fromIntegral y
              , sin (fromIntegral x / 2) + cos (fromIntegral y / 2)
              )
            | x <- [0 .. 4 :: Int]
            , y <- [0 .. 4 :: Int]
            ]
        labels = [oneDecimal z | (_, _, z) <- coords]
        df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- coords])
                , ("y", ColNum [y | (_, y, _) <- coords])
                , ("z", ColNum [z | (_, _, z) <- coords])
                , ("label", ColCat labels)
                ]
        labelLayer =
            (defLayer GeomText)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesLabel = Just (ColumnRef "label")
                        }
                , layerStat = StatIdentity
                }
     in heatmap
            { chartData = df
            , chartLayers = chartLayers heatmap <> [labelLayer]
            , chartTitle = Just "Annotated heatmap"
            }

oneDecimal :: Double -> Text
oneDecimal v =
    let rounded = fromIntegral (round (v * 10) :: Int) / 10 :: Double
     in Text.pack (show rounded)

------------------------------------------------------------------------
-- Financial
------------------------------------------------------------------------

{- | Funnel: five sales-pipeline stages with shrinking counts. Built
from horizontal bars (CoordFlip + GeomBar).
-}
funnel :: Chart
funnel =
    let xs = [0, 1, 2, 3, 4] :: [Double]
        ys = [1000, 720, 480, 220, 120] :: [Double]
        df = fromColumns [("stage", ColNum xs), ("count", ColNum ys)]
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

{- | Waterfall: incremental deltas making up a final total. Built from
a stacked bar where the base of each bar is the running total up
to that step (encoded as a per-row __ybase via a manual second
layer; for the IR we just place each delta as a bar with its
visible y range).
-}
waterfall :: Chart
waterfall =
    let xs = [0, 1, 2, 3, 4, 5] :: [Double]
        -- starting total, three positive deltas, one negative, ending total
        ystart = [0, 100, 130, 180, 175, 0] :: [Double]
        yend = [100, 130, 180, 175, 195, 195] :: [Double]
        -- We render the bars by reading the explicit __ybase column,
        -- which the bar geom already understands (Phase 7a wrote that
        -- convention).
        df =
            fromColumns
                [ ("x", ColNum xs)
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
