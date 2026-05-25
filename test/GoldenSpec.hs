{-# LANGUAGE OverloadedStrings #-}

module GoldenSpec (spec) where

import Data.Text (Text)

import Golden (goldenText)
import Granite.Chart (lineChart, scatterChart)
import Granite.Color (Color (..))
import Granite.Data.Frame (Column (..))
import Granite.Render.Pipeline (renderChartSvg, renderChartTerminal)
import Granite.Render.Scene (
    Mark (..),
    Point (..),
    Rect (..),
    Scene (..),
    Style (..),
    TextStyle (..),
    defaultStyle,
    defaultTextStyle,
 )
import Granite.Render.Svg qualified as Svg
import Granite.Render.Terminal qualified as Terminal
import Granite.Spec (
    BinSpec (..),
    Chart (..),
    ColorSpec (..),
    ColumnRef (..),
    Coord (..),
    Facet (..),
    FacetScales (..),
    Geom (..),
    Layer (..),
    LogBase (..),
    Mapping (..),
    PolarAes (..),
    PolarDir (..),
    Scale (..),
    Scales (..),
    Size (..),
    Stat (..),
    defLayer,
    defScaleOpts,
    defScales,
    emptyChart,
    emptyMapping,
    fromColumns,
    scaleY,
 )
import Test.Hspec

------------------------------------------------------------------------
-- Fixtures
------------------------------------------------------------------------

quadraticSeries :: Text -> [(Double, Double)] -> (Text, [(Double, Double)])
quadraticSeries n pts = (n, pts)

twoSeriesScatter :: Chart
twoSeriesScatter =
    scatterChart
        [ quadraticSeries "A" [(0, 0), (1, 1), (2, 4), (3, 9), (4, 16)]
        , quadraticSeries "B" [(0, 1), (1, 3), (2, 6), (3, 10), (4, 15)]
        ]
        (Just "Random points")

-- A scatter whose point opacity is driven by a numeric column via 'aesAlpha':
-- low weight -> faint, high weight -> opaque. Locks in per-point alpha.
alphaScatter :: Chart
alphaScatter =
    let df =
            fromColumns
                [ ("x", ColNum [0, 1, 2, 3, 4])
                , ("y", ColNum [0, 1, 2, 3, 4])
                , ("w", ColNum [1, 2, 3, 4, 5])
                ]
        layer =
            (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesAlpha = Just (ColumnRef "w")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Alpha by weight"
            , chartSize = SizeChars 40 14
            }

singleSeriesLine :: Chart
singleSeriesLine =
    lineChart
        [("sine", [(x, sin x) | x <- [0, 0.5 .. 6.0]])]
        (Just "sin(x)")

logYScatter :: Chart
logYScatter =
    twoSeriesScatter
        { chartScales = defScales{scaleY = SLog Base10 defScaleOpts}
        , chartTitle = Just "Log Y"
        }

responsiveScatter :: Chart
responsiveScatter =
    twoSeriesScatter
        { chartSize = SizeResponsive 1.6
        , chartTitle = Just "Responsive"
        }

reverseYLine :: Chart
reverseYLine =
    singleSeriesLine
        { chartScales = defScales{scaleY = SReverse (scaleY defScales)}
        , chartTitle = Just "Reverse Y"
        }

flippedLine :: Chart
flippedLine =
    lineChart
        [("y=2x", [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)])]
        (Just "CoordFlip")
        `withCoord` CoordFlip
  where
    withCoord c k = c{chartCoord = k}

-- A faceted scatter: same data spread across three panels by the
-- @series@ column. Tests FacetWrap layout + per-panel chrome.
facetedScatter :: Chart
facetedScatter =
    let pts =
            [ (0 :: Double, 1 :: Double, "A" :: Text)
            , (1, 4, "A")
            , (2, 9, "A")
            , (3, 16, "A")
            , (0, 0, "B")
            , (1, 2, "B")
            , (2, 4, "B")
            , (3, 6, "B")
            , (0, 5, "C")
            , (1, 4, "C")
            , (2, 3, "C")
            , (3, 2, "C")
            ]
        xs = [x | (x, _, _) <- pts]
        ys = [y | (_, y, _) <- pts]
        gs = [g | (_, _, g) <- pts]
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("y", ColNum ys)
                , ("series", ColCat gs)
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

-- A vertical bar chart with pre-aggregated heights. Uses StatIdentity
-- (the ggplot @stat="identity"@ convention) so the Y values pass
-- through unchanged. Exercises GeomBar.
barBasic :: Chart
barBasic =
    let xs = [0, 1, 2, 3] :: [Double]
        ys = [12, 7, 18, 9] :: [Double]
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
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

-- Histogram of 50 numeric values bucketed into 8 bins. Exercises
-- StatBin + GeomHistogram. aesY points to the @count@ column that
-- StatBin emits, so the bar heights track the bin counts.
histBasic :: Chart
histBasic =
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

-- Boxplot summarising three groups (A/B/C). Exercises StatBoxplot +
-- GeomBoxplot.
boxBasic :: Chart
boxBasic =
    let pts =
            [ (0 :: Double, v)
            | v <- [1, 2, 3, 4, 5, 6, 7, 8, 9 :: Double]
            ]
                <> [(1, v) | v <- [3, 4, 5, 6, 7, 8, 9, 10, 11 :: Double]]
                <> [(2, v) | v <- [5, 6, 7, 8, 9, 10, 11, 12, 13 :: Double]]
        df =
            fromColumns
                [ ("g", ColNum [g | (g, _) <- pts])
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

-- Pie chart of 4 slices. Exercises GeomArc.
pieBasic :: Chart
pieBasic =
    let df =
            fromColumns
                [ ("slice", ColCat ["A", "B", "C", "D"])
                , ("value", ColNum [40, 25, 20, 15])
                ]
        layer =
            (defLayer GeomArc)
                { layerMapping =
                    emptyMapping{aesY = Just (ColumnRef "value")}
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Pie"
            , chartSize = SizeChars 30 16
            }

-- A continuous-fill heatmap: GeomTile coloured by a numeric column through a
-- 'SColorContinuous' scaleFill (blue -> white -> red). Exercises the continuous
-- tile colour path + the suppressed tile legend.
contHeatmap :: Chart
contHeatmap =
    let coords =
            [ (fromIntegral x, fromIntegral y, fromIntegral (x + y) :: Double)
            | x <- [0 .. 3 :: Int]
            , y <- [0 .. 3 :: Int]
            ]
        df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- coords])
                , ("y", ColNum [y | (_, y, _) <- coords])
                , ("z", ColNum [z | (_, _, z) <- coords])
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
     in emptyChart
            { chartData = df
            , chartLayers = [tile]
            , chartScales =
                defScales
                    { scaleFill =
                        Just (SColorContinuous [Hex "#2166ac", Hex "#f7f7f7", Hex "#b2182b"])
                    }
            , chartTitle = Just "Continuous heatmap"
            , chartSize = SizeChars 40 14
            }

-- A four-petal rose in polar coordinates: r = |sin(2θ)| sampled at 33
-- angular positions. Locks in the polar projector + polar chrome.
polarRose :: Chart
polarRose =
    let nSteps = 32 :: Int
        pts =
            [ (theta, abs (sin (2 * theta)))
            | i <- [0 .. nSteps]
            , let theta = (fromIntegral i / fromIntegral nSteps) * 2 * pi
            ]
        base = lineChart [("r = |sin(2θ)|", pts)] (Just "Polar rose")
     in base
            { chartCoord = CoordPolar ThetaX 0 PolarCCW
            , chartSize = SizeChars 40 20
            }

noTitleScatter :: Chart
noTitleScatter = twoSeriesScatter{chartTitle = Nothing}

emptyChartFixture :: Chart
emptyChartFixture = emptyChart

-- A scene mixing every primitive mark; locked in once so future renderer
-- changes have to be intentional.
mixedPrimitivesScene :: Scene
mixedPrimitivesScene =
    Scene
        { sceneWidth = 240
        , sceneHeight = 120
        , sceneMarks =
            [ MRect (Rect 10 10 40 30) defaultStyle{styleFill = Just BrightBlue}
            , MCircle (Point 120 50) 12 defaultStyle{styleFill = Just BrightRed}
            , MPolyline
                [Point 20 100, Point 80 80, Point 140 90, Point 220 70]
                defaultStyle{styleStroke = Just BrightGreen, styleStrokeWidth = 2}
            , MText (Point 120 25) "Primitives" defaultTextStyle{textFill = BrightYellow}
            , MArc (Point 200 50) 20 0 1.5 defaultStyle{styleFill = Just BrightMagenta}
            ]
        }

------------------------------------------------------------------------
-- Spec
------------------------------------------------------------------------

spec :: Spec
spec = describe "Golden charts (run GRANITE_BLESS_GOLDEN=1 to refresh)" $ do
    describe "Scatter" $ do
        it "two-series scatter renders to expected terminal output" $
            goldenText "scatter-basic.txt" (renderChartTerminal twoSeriesScatter)

        it "two-series scatter renders to expected SVG output" $
            goldenText "scatter-basic.svg" (renderChartSvg twoSeriesScatter)

        it "no-title scatter renders to expected SVG output" $
            goldenText "scatter-no-title.svg" (renderChartSvg noTitleScatter)

        it "alpha-mapped scatter renders to expected SVG output" $
            goldenText "scatter-alpha.svg" (renderChartSvg alphaScatter)

    describe "Line chart" $ do
        it "single-series line renders to expected terminal output" $
            goldenText "line-basic.txt" (renderChartTerminal singleSeriesLine)

        it "single-series line renders to expected SVG output" $
            goldenText "line-basic.svg" (renderChartSvg singleSeriesLine)

    describe "Scales" $ do
        it "log-Y scatter renders to expected SVG output" $
            goldenText "scatter-log-y.svg" (renderChartSvg logYScatter)

        it "reverse-Y line renders to expected SVG output" $
            goldenText "line-reverse-y.svg" (renderChartSvg reverseYLine)

    describe "Coord" $ do
        it "CoordFlip line renders to expected terminal output" $
            goldenText "line-coord-flip.txt" (renderChartTerminal flippedLine)

        it "CoordFlip line renders to expected SVG output" $
            goldenText "line-coord-flip.svg" (renderChartSvg flippedLine)

        it "Polar rose renders to expected terminal output" $
            goldenText "polar-rose.txt" (renderChartTerminal polarRose)

        it "Polar rose renders to expected SVG output" $
            goldenText "polar-rose.svg" (renderChartSvg polarRose)

    describe "Facets" $ do
        it "FacetWrap by series renders to expected terminal output" $
            goldenText "facet-wrap.txt" (renderChartTerminal facetedScatter)

        it "FacetWrap by series renders to expected SVG output" $
            goldenText "facet-wrap.svg" (renderChartSvg facetedScatter)

    describe "Geoms (Phase 7b)" $ do
        it "bar chart renders to expected terminal output" $
            goldenText "bar-basic.txt" (renderChartTerminal barBasic)
        it "bar chart renders to expected SVG output" $
            goldenText "bar-basic.svg" (renderChartSvg barBasic)

        it "histogram renders to expected terminal output" $
            goldenText "hist-basic.txt" (renderChartTerminal histBasic)
        it "histogram renders to expected SVG output" $
            goldenText "hist-basic.svg" (renderChartSvg histBasic)

        it "boxplot renders to expected terminal output" $
            goldenText "box-basic.txt" (renderChartTerminal boxBasic)
        it "boxplot renders to expected SVG output" $
            goldenText "box-basic.svg" (renderChartSvg boxBasic)

        it "pie chart renders to expected SVG output" $
            goldenText "pie-basic.svg" (renderChartSvg pieBasic)

        it "continuous-fill heatmap renders to expected SVG output" $
            goldenText "heatmap-continuous.svg" (renderChartSvg contHeatmap)

    describe "Sizing" $ do
        it "responsive scatter renders to expected SVG output" $
            goldenText "scatter-responsive.svg" (renderChartSvg responsiveScatter)

    describe "Empty chart" $ do
        it "empty chart renders to expected SVG output" $
            goldenText "empty.svg" (renderChartSvg emptyChartFixture)

    describe "Scene primitives" $ do
        it "mixed-marks Scene → terminal" $
            goldenText "primitives.txt" (Terminal.renderScene mixedPrimitivesScene)

        it "mixed-marks Scene → SVG" $
            goldenText "primitives.svg" (Svg.renderScene mixedPrimitivesScene)
