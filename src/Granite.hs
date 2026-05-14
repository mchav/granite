{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com
Stability   : experimental
Portability : POSIX

A terminal-based plotting library that renders beautiful charts using Unicode
Braille characters and ANSI colors. Granite provides a variety of chart types
including scatter plots, line graphs, bar charts, pie charts, histograms,
heatmaps, and box plots.

= Basic Usage

Create a simple scatter plot:

@
{\-# LANGUAGE OverloadedStrings #-\}
import Granite
import Data.Text.IO as T

main = do
  let points = [(x, sin x) | x <- [0, 0.1 .. 6.28]]
      chart = scatter [series "sin(x)" points] defPlot
  T.putStrLn chart
@

= Customization

Plots can be customized using record update syntax:

@
let customPlot = defPlot
      { widthChars = 80
      , heightChars = 30
      , plotTitle = "My Chart"
      , legendPos = LegendBottom
      }
@

= Terminal Requirements

This library requires a terminal that supports:

  * Unicode (specifically Braille patterns U+2800-U+28FF)
  * ANSI color codes
  * Monospace font with proper Braille character rendering
-}
module Granite (
    -- * Plot Configuration
    Plot (..),
    defPlot,
    LegendPos (..),

    -- * Formatting
    Color (..),
    LabelFormatter,
    AxisEnv (..),

    -- * Data Preparation
    series,
    bins,
    Bins (..),

    -- * Chart Types
    histogram,
    bars,
    scatter,
    pie,
    stackedBars,
    heatmap,
    lineGraph,
    boxPlot,

    -- * Plotly-Express-style helpers
    area,
    ribbon,
    density,
    errorBars,
    funnel,
    polarLine,
    waterfall,
    distPlot,
) where

import Data.Bits (xor, (.&.))
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showEFloat, showFFloat)
import Text.Printf

import Granite.Color (Color (..), paint, paletteColors, pieColors)
import Granite.Internal.LegacyChart qualified as LC
import Granite.Internal.Util (
    addAt,
    angleWithin,
    clamp,
    ellipsisize,
    eps,
    gridWidth,
    justifyRight,
    maximum',
    minimum',
    mod',
    normalize,
    quartiles,
    setAt,
    ticks1D,
    updateAt,
    wcswidth,
 )
import Granite.Render.Pipeline (renderChartTerminal)
import Granite.Render.Terminal (
    Canvas (..),
    fillDotsC,
    lineDotsC,
    newCanvas,
    renderCanvas,
    setDotC,
 )

-- | Position of the legend in the plot.
data LegendPos
    = -- | Display legend on the right side of the plot
      LegendRight
    | -- | Display legend below the plot
      LegendBottom
    | -- | Do not display legend.
      LegendNone
    deriving (Eq, Show)

{- | Plot configuration parameters.

Controls the appearance and layout of generated charts.
-}
data Plot = Plot
    { widthChars :: Int
    -- ^ Width of the plot area in terminal characters (default: 60)
    , heightChars :: Int
    -- ^ Height of the plot area in terminal characters (default: 20)
    , leftMargin :: Int
    -- ^ Space reserved for y-axis labels (default: 6)
    , bottomMargin :: Int
    -- ^ Space reserved for x-axis labels (default: 2)
    , titleMargin :: Int
    -- ^ Space above the plot for the title (default: 1)
    , xBounds :: (Maybe Double, Maybe Double)
    {- ^ Optional manual x-axis bounds (min, max).
    'Nothing' uses automatic bounds with 5% padding.
    -}
    , yBounds :: (Maybe Double, Maybe Double)
    {- ^ Optional manual y-axis bounds (min, max).
    'Nothing' uses automatic bounds with 5% padding.
    -}
    , plotTitle :: Text
    -- ^ Title displayed above the plot (default: empty)
    , legendPos :: LegendPos
    -- ^ Position of the legend (default: 'LegendRight')
    , colorPalette :: [Color]
    -- ^ Color palette that'll be used by the plot.
    , xFormatter :: LabelFormatter
    -- ^ Formatter for x-axis labels.
    , yFormatter :: LabelFormatter
    -- ^ Formatter for y-axis labels.
    , xNumTicks :: Int
    -- ^ Number of ticks on the x axis.
    , yNumTicks :: Int
    -- ^ Number of ticks on the y axis.
    }

{- | Default plot configuration.

Creates a 60×20 character plot with reasonable defaults:

@
defPlot = Plot
  { widthChars   = 60
  , heightChars  = 20
  , leftMargin   = 6
  , bottomMargin = 2
  , titleMargin  = 1
  , xBounds      = (Nothing, Nothing)
  , yBounds      = (Nothing, Nothing)
  , plotTitle    = ""
  , legendPos    = LegendRight
  , colorPalette = [ BrightBlue, BrightMagenta, BrightCyan, BrightGreen, BrightYellow, BrightRed, BrightWhite, BrightBlack]
  , xFormatter   = \\ _ _ v -> show v
  , yFormatter   = \\ _ _ v -> show v
  , xNumTicks    = 2
  , yNumTicks    = 2
  }
@
-}
defPlot :: Plot
defPlot =
    Plot
        { widthChars = 60
        , heightChars = 20
        , leftMargin = 6
        , bottomMargin = 2
        , titleMargin = 1
        , xBounds = (Nothing, Nothing)
        , yBounds = (Nothing, Nothing)
        , plotTitle = ""
        , legendPos = LegendRight
        , colorPalette = paletteColors
        , xFormatter = fmt
        , yFormatter = fmt
        , xNumTicks = 3
        , yNumTicks = 3
        }

{- | Axis-aware, width-limited, tick-label formatter.

Given:

   * axis context
   * a per-tick width budget (in terminal cells)
   * and the raw tick value.

returns the label to render.
-}
type LabelFormatter =
    -- | Axis context (domain, tick index/count, etc)
    AxisEnv ->
    -- | Slot width budget in characters for this tick.
    Int ->
    -- | Raw data value for the tick
    Double ->
    -- | Rendered label (if it doesn't fit in the slot it will be truncated)
    Text.Text

-- | What the formatter gets to know about the axis/ticks
data AxisEnv = AxisEnv
    { domain :: (Double, Double)
    -- ^ min/max of the axis in data space
    , tickIndex :: Int
    -- ^ index of THIS tick [0..tickCount-1]
    , tickCount :: Int
    -- ^ total number of ticks
    }

{- | Create a named data series for multi-series plots.

@
let s1 = series "Dataset A" [(1,2), (2,4), (3,6)]
    s2 = series "Dataset B" [(1,3), (2,5), (3,7)]
    chart = scatter [s1, s2] defPlot
@
-}
series ::
    -- | Name of the series (appears in legend)
    Text ->
    -- | List of (x, y) data points
    [(Double, Double)] ->
    (Text, [(Double, Double)])
series = (,)

{- | Create a scatter plot from multiple data series.

Each series is rendered with a different color and pattern.
Points are plotted using Braille characters for sub-character resolution.

==== __Example__

@
let points1 = [(x, x^2) | x <- [-3, -2.5 .. 3]]
    points2 = [(x, 2*x + 1) | x <- [-3, -2.5 .. 3]]
    chart = scatter [series "y = x²" points1,
                     series "y = 2x + 1" points2] defPlot
@
-}
scatter ::
    -- | List of named data series
    [(Text, [(Double, Double)])] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
scatter sers cfg =
    let wC = widthChars cfg
        hC = heightChars cfg
        plotC = newCanvas wC hC
        (xmin, xmax, ymin, ymax) = boundsXY cfg (concatMap snd sers)
        sx x =
            clamp 0 (wC * 2 - 1) $
                round ((x - xmin) / (xmax - xmin + eps) * fromIntegral (wC * 2 - 1))
        sy y =
            clamp 0 (hC * 4 - 1) $
                round ((ymax - y) / (ymax - ymin + eps) * fromIntegral (hC * 4 - 1))
        pats = cycle palette
        cols = cycle (colorPalette cfg)
        withSty = zipWith3 (\(n, ps) p c -> (n, ps, p, c)) sers pats cols
        drawOne c0 (_name, pts, pat, col) =
            List.foldl'
                ( \c (x, y) ->
                    let xd = sx x; yd = sy y
                     in if ink pat xd yd then setDotC c xd yd (Just col) else c
                )
                c0
                pts
        cDone = List.foldl' drawOne plotC withSty
        ax = axisify cfg cDone (xmin, xmax) (ymin, ymax)
        legend =
            legendBlock
                (legendPos cfg)
                (leftMargin cfg + widthChars cfg)
                [(n, p, col) | (n, _, p, col) <- withSty]
     in drawFrame cfg ax legend

{- | Create a line graph connecting data points.

Similar to 'scatter' but connects consecutive points with lines.
Points are automatically sorted by x-coordinate before connecting.

==== __Example__

@
let sine = [(x, sin x) | x <- [0, 0.1 .. 2*pi]]
    cosine = [(x, cos x) | x <- [0, 0.1 .. 2*pi]]
    chart = lineGraph [series "sin" sine, series "cos" cosine] defPlot
@
-}
lineGraph ::
    -- | List of named data series
    [(Text, [(Double, Double)])] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
lineGraph sers cfg =
    let wC = widthChars cfg
        hC = heightChars cfg
        plotC = newCanvas wC hC
        (xmin, xmax, ymin, ymax) = boundsXY cfg (concatMap snd sers)
        sx x =
            clamp 0 (wC * 2 - 1) $
                round ((x - xmin) / (xmax - xmin + eps) * fromIntegral (wC * 2 - 1))
        sy y =
            clamp 0 (hC * 4 - 1) $
                round ((ymax - y) / (ymax - ymin + eps) * fromIntegral (hC * 4 - 1))

        cols = cycle (colorPalette cfg)
        withSty = zip sers cols

        drawSeries ((_name, pts), col) c0 =
            let sortedPts = List.sortOn fst pts
                dotPairs = zip sortedPts (drop 1 sortedPts)
             in List.foldl'
                    ( \c ((x1, y1), (x2, y2)) ->
                        lineDotsC (sx x1, sy y1) (sx x2, sy y2) (Just col) c
                    )
                    c0
                    dotPairs

        cDone = List.foldl' (flip drawSeries) plotC withSty
        ax :: Text
        ax = axisify cfg cDone (xmin, xmax) (ymin, ymax)
        legend :: Text
        legend =
            legendBlock
                (legendPos cfg)
                (leftMargin cfg + widthChars cfg)
                [(n, Solid, col) | ((n, _), col) <- withSty]
     in drawFrame cfg ax legend

{- | Create a bar chart from categorical data.

Each bar is colored differently and labeled with its category name.

==== __Example__

@
let fruits = [(\"Apple\", 45.2), (\"Banana\", 38.1), (\"Orange\", 52.7)]
    chart = bars fruits defPlot { plotTitle = "Fruit Sales" }
@
-}
bars ::
    -- | List of (category, value) pairs
    [(Text, Double)] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
bars kvs cfg =
    let wC = widthChars cfg
        hC = heightChars cfg
        (catNames, vals) = unzip kvs
        vmax = maximum' (map abs vals)

        cats :: [(Text, Double, Color)]
        cats =
            [ (name, abs v / vmax, col)
            | ((name, v), col) <- zip kvs (cycle (colorPalette cfg))
            ]

        nCats = min wC (length cats)

        (base, extra) =
            if nCats == 0 then (0, 0) else (wC `div` nCats, wC - wC `div` nCats * nCats)
        widths = [base + (if i < extra then 1 else 0) | i <- [0 .. nCats - 1]]

        catGroups :: [[(String, Maybe Color)]]
        catGroups =
            [ replicate w (colGlyphs hC f, Just col)
            | ((_, f, col), w) <- zip cats widths
            ]

        gutterCol = (replicate hC ' ', Nothing)
        columns = List.intercalate [gutterCol] catGroups

        grid :: [[(Char, Maybe Color)]]
        grid =
            [ [(glyphs !! y, mc) | (glyphs, mc) <- columns]
            | y <- [0 .. hC - 1]
            ]

        ax =
            axisifyGrid
                cfg
                grid
                (0, fromIntegral (max 1 nCats))
                (0, vmax)
                catNames
                (fmap (+ 1) (listToMaybe widths))
        legendWidth = leftMargin cfg + 1 + gridWidth grid
        legend =
            legendBlock
                (legendPos cfg)
                legendWidth
                [(name, Checker, col) | (name, _, col) <- cats]
     in drawFrame cfg ax legend

{- | Create a stacked bar chart.

Each category can have multiple stacked components.

==== __Example__

@
let sales = [(\"Q1\", [(\"Product A\", 100), (\"Product B\", 150)]),
             (\"Q2\", [(\"Product A\", 120), (\"Product B\", 180)])]
    chart = stackedBars sales defPlot
@
-}
stackedBars ::
    -- | Categories with stacked components
    [(Text, [(Text, Double)])] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
stackedBars categories cfg =
    let wC = widthChars cfg
        hC = heightChars cfg

        seriesNames = case categories of
            [] -> []
            (c : _) -> map fst (snd c)

        totals = [sum (map snd series') | (_, series') <- categories]
        maxHeight = maximum (1e-12 : totals)

        nCats = length categories
        (base, extra) =
            if nCats == 0
                then (0, 0)
                else (wC `div` nCats, wC - wC `div` nCats * nCats)
        widths = [base + (if i < extra then 1 else 0) | i <- [0 .. nCats - 1]]

        cols = cycle (colorPalette cfg)
        seriesColors = zip seriesNames cols

        makeBar (_, series') width =
            let cumHeights = scanl (+) 0 [v / maxHeight | (_, v) <- series']
                segments = zip3 (map fst series') cumHeights (drop 1 cumHeights)

                makeColumn :: [(Char, Maybe Color)]
                makeColumn =
                    [ let heightFromBottom = fromIntegral (hC - y) / fromIntegral hC
                          findSegment [] = (' ', Nothing)
                          findSegment ((name, bottom, top) : rest) =
                            if heightFromBottom > bottom && heightFromBottom <= top
                                then ('█', lookup name seriesColors)
                                else findSegment rest
                       in findSegment segments
                    | y <- [0 .. hC - 1]
                    ]
             in replicate width makeColumn

        gutterCol = replicate hC (' ', Nothing)
        allBars = zipWith makeBar categories widths
        columns = List.intercalate [gutterCol] allBars

        grid = [[col !! y | col <- columns] | y <- [0 .. hC - 1]]

        ax :: Text
        ax =
            axisifyGrid
                cfg
                grid
                (0, fromIntegral (max 1 nCats))
                (0, maxHeight)
                (map fst categories)
                (fmap (+ 1) (listToMaybe widths))
        legend :: Text
        legend =
            legendBlock
                (legendPos cfg)
                ( leftMargin cfg
                    + 1
                    + gridWidth grid
                )
                [(name, Solid, col) | (name, col) <- seriesColors]
     in drawFrame cfg ax legend

-- | Defines the binning parameters.
data Bins = Bins
    { nBins :: Int
    , lo :: Double
    , hi :: Double
    }
    deriving (Eq, Show)

{- | Create a bin configuration for histograms.

@
bins 10 0 100  -- 10 bins from 0 to 100
bins 20 (-5) 5 -- 20 bins from -5 to 5
@
-}
bins :: Int -> Double -> Double -> Bins
bins n a b = Bins (max 1 n) (min a b) (max a b)

{- | Create a histogram from numerical data.

Data is binned according to the provided 'Bins' configuration.

==== __Example__

@
import System.Random

-- Generate random normal-like distribution
let values = take 1000 $ randomRs (0, 100) gen
    chart = histogram (bins 20 0 100) values defPlot
@
-}
histogram ::
    -- | Binning configuration
    Bins ->
    -- | Raw data values to bin
    [Double] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
histogram (Bins n a b) xs cfg =
    let step = (b - a) / fromIntegral n
        binIx x = clamp 0 (n - 1) $ floor ((x - a) / step)
        counts =
            List.foldl'
                ( \acc x ->
                    if x < a || x > b
                        then acc
                        else addAt acc (binIx x) 1
                )
                (replicate n 0 :: [Int])
                xs
        maxC = fromIntegral (maximum (1 : counts))
        fracs0 = [fromIntegral c / maxC | c <- counts]

        wData = widthChars cfg
        hC = heightChars cfg
        colsF = resampleToWidth wData fracs0

        dataCols = [(colGlyphs hC f, Just BrightCyan) | f <- colsF]
        grid :: [[(Char, Maybe Color)]]
        grid =
            [ [(fst col !! y, snd col) | col <- dataCols]
            | y <- [0 .. hC - 1]
            ]

        ax =
            axisifyGrid cfg grid (a, b) (0, maxC) [] Nothing
        legendWidth = leftMargin cfg + 1 + gridWidth grid
        legend = legendBlock (legendPos cfg) legendWidth [("count", Solid, BrightCyan)]
     in drawFrame cfg ax legend

{- | Create a pie chart showing proportions.

Values are normalized to sum to 100%. Negative values are treated as zero.

==== __Example__

@
let browsers = [(\"Chrome\", 65), (\"Firefox\", 20), (\"Safari\", 10), (\"Other\", 5)]
    chart = pie browsers defPlot { plotTitle = "Browser Market Share" }
@
-}
pie ::
    -- | List of (category, value) pairs
    [(Text, Double)] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
pie parts0 cfg =
    let parts = normalize parts0
        wC = widthChars cfg
        hC = heightChars cfg
        plotC = newCanvas wC hC
        wDots = wC * 2
        hDots = hC * 4
        r = min (wDots `div` 2 - 2) (hDots `div` 2 - 2)
        cx = wDots `div` 2
        cy = hDots `div` 2
        toAng p = p * 2 * pi
        wedges = scanl (\a (_, p) -> a + toAng p) 0 parts
        angles = zip wedges (drop 1 wedges)
        names = map fst parts
        cols = cycle pieColors
        withP :: [(Text, (Double, Double), Color)]
        withP = zip3 names angles cols

        drawOne (_name, (a0, a1), col) c0 =
            let inside x y =
                    let dx = fromIntegral (x - cx)
                        dy = fromIntegral (cy - y)
                        rr2 = dx * dx + dy * dy
                        r2 = fromIntegral (r * r)
                        ang = atan2 dy dx `mod'` (2 * pi)
                     in rr2 <= r2 && angleWithin ang a0 a1
             in fillDotsC (cx - r, cy - r) (cx + r, cy + r) inside (Just col) c0

        cDone = List.foldl' (flip drawOne) plotC withP
        ax = axisify cfg cDone (0, 1) (0, 1)
        legend =
            legendBlock
                (legendPos cfg)
                (leftMargin cfg + widthChars cfg)
                [(n, Solid, col) | (n, _, col) <- withP]
     in drawFrame cfg ax legend

{- | Create a heatmap visualization of a 2D matrix.

Values are mapped to a color gradient from blue (low) to red (high).

==== __Example__

@
let matrix = [[x * y | x <- [1..10]] | y <- [1..10]]
    chart = heatmap matrix defPlot { plotTitle = "Multiplication Table" }
@
-}
heatmap ::
    -- | 2D matrix of values (rows × columns)
    [[Double]] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
heatmap matrix cfg =
    let rows = length matrix
        cols = gridWidth matrix

        allVals = concat matrix
        vmin = if null allVals then 0 else minimum' allVals
        vmax = if null allVals then 1 else maximum' allVals
        vrange = vmax - vmin

        intensityColors =
            [ Blue
            , BrightBlue
            , Cyan
            , BrightCyan
            , Green
            , BrightGreen
            , Yellow
            , BrightYellow
            , Magenta
            , BrightRed
            , Red
            ]

        colorForValue v =
            if vrange < eps
                then Green
                else
                    let norm = clamp 0 1 ((v - vmin) / vrange)
                        idx = floor (norm * fromIntegral (length intensityColors - 1))
                        idx' = clamp 0 (length intensityColors - 1) idx
                     in intensityColors !! idx'

        displayGrid =
            [ [ let
                    matrixRow = min (rows - 1) ((plotH - 1 - i) * rows `div` plotH)
                    matrixCol = min (cols - 1) (j * cols `div` plotW)
                    val = matrix !! matrixRow !! matrixCol
                 in
                    ('█', Just (colorForValue val))
              | j <- [0 .. plotW - 1]
              ]
            | i <- [0 .. plotH - 1]
            ]

        plotW = widthChars cfg
        plotH = heightChars cfg

        colLabels = [Text.pack (show i) | i <- [0 .. cols - 1]]
        rowLabels = [Text.pack (show i) | i <- reverse [0 .. rows - 1]]

        cellHeight = fromIntegral plotH / fromIntegral rows
        yTicks =
            [ ( round @Double @Int (fromIntegral i * cellHeight + cellHeight / 2)
              , rowLabels !! i
              )
            | i <- [0 .. rows - 1]
            ]

        left = leftMargin cfg
        baseLbl = replicate plotH (Text.replicate left " ")
        yLabels =
            List.foldl'
                ( \acc (row, lbl) ->
                    setAt acc row (justifyRight left (ellipsisize left lbl))
                )
                baseLbl
                yTicks

        renderRow cells =
            Text.concat
                (fmap (\(ch, mc) -> maybe (Text.singleton ch) (`paint` ch) mc) cells)
        attachY = zipWith (\lbl cells -> lbl <> "│" <> renderRow cells) yLabels displayGrid

        xBar = Text.replicate left " " <> "└" <> Text.replicate plotW "─"
        xLine =
            placeGridLabels
                (Text.replicate (left + 1) " ")
                (plotW `div` cols)
                colLabels

        ax = Text.unlines (attachY <> [xBar, xLine])

        gradientLegend =
            Text.pack (printf "%.2f " vmin)
                <> Text.concat (fmap (`paint` '█') intensityColors)
                <> Text.pack (printf " %.2f" vmax)
     in drawFrame cfg ax gradientLegend

{- | Create a box plot showing statistical distributions.

Displays quartiles, median, and min/max values for each dataset.

==== __Example__

@
let data1 = [1.2, 2.3, 2.1, 3.4, 2.8, 4.1, 3.9]
    data2 = [5.1, 4.8, 6.2, 5.9, 7.1, 6.5, 5.5]
    chart = boxPlot [("Group A", data1), ("Group B", data2)] defPlot
@

The box plot displays:

  * Box: First quartile (Q1) to third quartile (Q3)
  * Line inside box: Median (Q2)
  * Whiskers: Minimum and maximum values
-}
boxPlot ::
    -- | Named datasets
    [(Text, [Double])] ->
    -- | Plot configuration
    Plot ->
    -- | Rendered chart as Text
    Text
boxPlot datasets cfg =
    let wC = widthChars cfg
        hC = heightChars cfg

        stats = [(name, quartiles vals) | (name, vals) <- datasets]

        allVals = concatMap snd datasets
        ymin = if null allVals then 0 else minimum' allVals - abs (minimum' allVals) * 0.1
        ymax = if null allVals then 1 else maximum' allVals + abs (maximum' allVals) * 0.1

        nBoxes = length datasets
        boxWidth = if nBoxes == 0 then 1 else max 1 (wC `div` (nBoxes * 2))
        spacing = if nBoxes <= 1 then 0 else (wC - boxWidth * nBoxes) `div` (nBoxes - 1)

        scaleY v =
            clamp 0 (hC - 1) $
                round ((ymax - v) / (ymax - ymin + eps) * fromIntegral (hC - 1))

        emptyGrid = replicate hC (replicate wC (' ', Nothing))

        drawBox grid (idx, (_name, (minV, q1, median, q3, maxV))) =
            let xStart = idx * (boxWidth + spacing)
                xMid = xStart + boxWidth `div` 2
                xEnd = xStart + boxWidth - 1

                minRow = scaleY minV
                q1Row = scaleY q1
                medRow = scaleY median
                q3Row = scaleY q3
                maxRow = scaleY maxV

                col = pieColors !! (idx `mod` length pieColors)

                grid1 = drawVLine grid xMid minRow q1Row '│' (Just col)
                grid2 = drawVLine grid1 xMid q3Row maxRow '│' (Just col)

                grid3 = drawHLine grid2 xStart xEnd q1Row '─' (Just col)
                grid4 = drawHLine grid3 xStart xEnd q3Row '─' (Just col)
                grid5 = drawVLine grid4 xStart q1Row q3Row '│' (Just col)
                grid6 = drawVLine grid5 xEnd q1Row q3Row '│' (Just col)

                grid7 = drawHLine grid6 xStart xEnd medRow '═' (Just col)

                grid8 = setGridChar grid7 xMid minRow '┴' (Just col)
                grid9 = setGridChar grid8 xMid maxRow '┬' (Just col)
             in grid9

        finalGrid = List.foldl' drawBox emptyGrid (zip [0 ..] stats)

        left = leftMargin cfg
        baseLbl = replicate hC (Text.replicate left " ")

        yTicks = ticks1D hC (yNumTicks cfg) (ymin, ymax) True
        yEnv n = AxisEnv (ymin, ymax) n (yNumTicks cfg)
        ySlot = max 1 left
        yLabels =
            List.foldl'
                ( \acc (row, v) ->
                    setAt acc row . ellipsisize left . justifyRight left $
                        yFormatter cfg (yEnv row) ySlot v
                )
                baseLbl
                yTicks

        renderRow cells =
            Text.concat
                (fmap (\(ch, mc) -> maybe (Text.singleton ch) (`paint` ch) mc) cells)
        attachY = zipWith (\lbl cells -> lbl <> "│" <> renderRow cells) yLabels finalGrid

        xBar = Text.replicate left " " <> "└" <> Text.replicate wC "─"

        xLine =
            List.foldl'
                ( \acc (idx, name) ->
                    let boxCenter = idx * (boxWidth + spacing) + boxWidth `div` 2
                        lblWidth = wcswidth name
                        lblStart = left + 1 + boxCenter - lblWidth `div` 2
                     in Text.take lblStart acc <> name <> Text.drop (lblStart + wcswidth name) acc
                )
                (Text.replicate (left + 1 + wC) " ")
                (zip [0 ..] (map fst datasets))

        ax = Text.unlines (attachY <> [xBar, xLine])

        legend =
            legendBlock
                (legendPos cfg)
                (leftMargin cfg + widthChars cfg)
                [ (name, Solid, pieColors !! (i `mod` length pieColors))
                | (i, (name, _)) <- zip [0 ..] stats
                ]
     in drawFrame cfg ax legend
  where
    drawVLine grid x y1 y2 ch col =
        let yStart = min y1 y2
            yEnd = max y1 y2
         in List.foldl' (\g y -> setGridChar g x y ch col) grid [yStart .. yEnd]

    drawHLine grid x1 x2 y ch col =
        let xStart = min x1 x2
            xEnd = max x1 x2
         in List.foldl' (\g x -> setGridChar g x y ch col) grid [xStart .. xEnd]

    setGridChar grid x y ch col =
        updateAt grid y (\row -> setAt row x (ch, col))

data Pat = Solid | Checker | DiagA | DiagB | Sparse deriving (Eq, Show)

ink :: Pat -> Int -> Int -> Bool
ink Solid _ _ = True
ink Checker x y = (x `xor` y) .&. 1 == 0
ink DiagA x y = (x + y) `mod` 3 /= 1
ink DiagB x y = (x - y) `mod` 3 /= 1
ink Sparse x y = x .&. 1 == 0 && y `mod` 3 == 0

palette :: [Pat]
palette = [Solid, Checker, DiagA, DiagB, Sparse]

fmt :: AxisEnv -> Int -> Double -> Text
fmt _ _ v
    | abs v >= 10000 || abs v < 0.01 && v /= 0 =
        Text.pack (showEFloat (Just 1) v "")
    | otherwise = Text.pack (showFFloat (Just 1) v "")

drawFrame :: Plot -> Text -> Text -> Text
drawFrame cfg contentWithAxes legendBlockStr =
    Text.unlines $
        filter
            (not . Text.null)
            [plotTitle cfg, contentWithAxes, legendBlockStr]

slotBudget :: Int -> Int -> Int
slotBudget plotPixels numTicks =
    max 1 (plotPixels `div` max 1 numTicks)

axisify :: Plot -> Canvas -> (Double, Double) -> (Double, Double) -> Text
axisify cfg c (xmin, xmax) (ymin, ymax) =
    let plotW = cW c
        plotH = cH c
        left = leftMargin cfg
        pad = Text.replicate left " "

        yTicks :: [(Int, Double)]
        yTicks = ticks1D plotH (yNumTicks cfg) (ymin, ymax) True

        baseLbl :: [Text]
        baseLbl = replicate plotH pad

        yEnv n = AxisEnv (ymin, ymax) n 3
        ySlot = max 1 left
        yLabels =
            List.foldl'
                ( \acc (row, v) ->
                    setAt acc row
                        . ellipsisize left
                        . justifyRight left
                        $ yFormatter cfg (yEnv row) ySlot v
                )
                baseLbl
                yTicks

        canvasLines = Text.lines (renderCanvas c)
        attachY = zipWith (\lbl line -> lbl <> "│" <> line) yLabels canvasLines

        xBar = pad <> "└" <> Text.replicate plotW "─"

        xTicks :: [(Int, Double)]
        xTicks = ticks1D plotW (xNumTicks cfg) (xmin, xmax) False

        xEnv n = AxisEnv (xmin, xmax) n 3
        slotW = slotBudget plotW (max 1 (length xTicks))
        xLine =
            placeLabels
                (Text.replicate (left + 1 + plotW) " ")
                (left + 1)
                [(x, xFormatter cfg (xEnv i) slotW v) | (i, (x, v)) <- zip [0 ..] xTicks]
     in Text.unlines (attachY <> [xBar, xLine])

axisifyGrid ::
    Plot ->
    [[(Char, Maybe Color)]] ->
    (Double, Double) ->
    (Double, Double) ->
    [Text] ->
    Maybe Int ->
    Text
axisifyGrid cfg grid (xmin, xmax) (ymin, ymax) categories w =
    let plotH = length grid
        plotW = gridWidth grid
        left = leftMargin cfg
        pad = Text.replicate left " "

        yTicks = ticks1D plotH (yNumTicks cfg) (ymin, ymax) True
        baseLbl = List.replicate plotH pad

        yEnv n = AxisEnv (ymin, ymax) n (yNumTicks cfg)
        ySlot = max 1 left
        yLabels =
            List.foldl'
                ( \acc (row, v) ->
                    setAt acc row
                        . ellipsisize left
                        . justifyRight left
                        $ yFormatter cfg (yEnv row) ySlot v
                )
                baseLbl
                yTicks

        renderRow :: [(Char, Maybe Color)] -> Text
        renderRow cells =
            Text.concat
                (fmap (\(ch, mc) -> maybe (Text.singleton ch) (`paint` ch) mc) cells)

        attachY = zipWith (\lbl cells -> lbl <> "│" <> renderRow cells) yLabels grid

        xBar = pad <> "└" <> Text.replicate plotW "─"

        hasCategories = not (null categories) && not (all Text.null categories)

        xLine =
            if hasCategories
                then
                    let slotW =
                            fromMaybe
                                ( slotBudget
                                    plotW
                                    (max 1 (xNumTicks cfg))
                                )
                                w
                        nSlots = plotW `div` slotW
                        xTicks = ticks1D plotW nSlots (xmin, xmax) False
                     in placeGridLabels
                            (Text.replicate (left + 1) " ")
                            slotW
                            (keepPercentiles (length categories) (length xTicks + 1) categories)
                else
                    let xTicks = ticks1D plotW (xNumTicks cfg) (xmin, xmax) False
                        xEnv i = AxisEnv (xmin, xmax) i (xNumTicks cfg)
                        slotW = slotBudget plotW (max 1 (length xTicks))
                     in placeLabels
                            (Text.replicate (left + 1 + plotW) " ")
                            (left + 1)
                            [(x, xFormatter cfg (xEnv i) slotW v) | (i, (x, v)) <- zip [0 ..] xTicks]
     in Text.unlines (attachY <> [xBar, xLine])

keepPercentiles :: Int -> Int -> [Text] -> [Text]
keepPercentiles n k xs
    | k <= 0 = []
    | null xs = replicate k ""
    | n <= 1 = replicate k ""
    | otherwise = map valueAt [0 .. k - 2] ++ [last xs]
  where
    m = length xs
    pairs :: [(Int, Text)]
    pairs =
        [ ( slotIx
          , xs !! srcIx
          )
        | i <- [0 .. n - 2]
        , let srcIx = (i * (m - 1)) `div` (n - 1)
        , let slotIx = (i * (k - 1)) `div` (n - 1)
        ]

    valueAt :: Int -> Text
    valueAt i = fromMaybe "" $ List.lookup i pairs

placeLabels :: Text -> Int -> [(Int, Text)] -> Text
placeLabels base off = List.foldl' place base
  where
    place :: Text -> (Int, Text) -> Text
    place acc (x, s) =
        let i = off + x
         in Text.take i acc <> s <> Text.drop (i + wcswidth s) acc

placeGridLabels :: Text -> Int -> [Text] -> Text
placeGridLabels base slotW = List.foldl' place base
  where
    place :: Text -> Text -> Text
    place acc s =
        let lblWidth = wcswidth s
            padding = max 0 ((slotW - lblWidth) `div` 2)
            centered = Text.replicate padding " " <> s
         in acc <> Text.take slotW (centered <> Text.replicate slotW " ")

legendBlock :: LegendPos -> Int -> [(Text, Pat, Color)] -> Text
legendBlock LegendBottom width entries =
    let cells = [sample pat col <> " " <> name | (name, pat, col) <- entries]
        line = Text.intercalate "   " cells
        pad =
            let vis = wcswidth line
             in if vis < width then Text.replicate ((width - vis) `div` 2) " " else ""
     in pad <> line
legendBlock LegendRight _ entries =
    Text.unlines $
        fmap (\(name, pat, col) -> sample pat col <> " " <> name) entries
legendBlock LegendNone _ _ = ""

sample :: Pat -> Color -> Text
sample p col =
    let c =
            List.foldl'
                ( \cv (dx, dy) -> if ink p dx dy then setDotC cv (dx `mod` 2) (dy `mod` 4) (Just col) else cv
                )
                (newCanvas 1 1)
                [(x, y) | y <- [0 .. 3], x <- [0 .. 1]]
        s = renderCanvas c
     in Text.dropWhileEnd (== '\n') s

boundsXY :: Plot -> [(Double, Double)] -> (Double, Double, Double, Double)
boundsXY cfg pts =
    let (xs, ys) = unzip pts
        xmin = minimum' xs
        xmax = maximum' xs
        ymin = minimum' ys
        ymax = maximum' ys
        padx = (xmax - xmin) * 0.05 + 1e-9
        pady = (ymax - ymin) * 0.05 + 1e-9
     in ( fromMaybe (xmin - padx) (fst (xBounds cfg))
        , fromMaybe (xmax + padx) (snd (xBounds cfg))
        , fromMaybe (ymin - pady) (fst (yBounds cfg))
        , fromMaybe (ymax + pady) (snd (yBounds cfg))
        )

blockChar :: Int -> Char
blockChar n = case clamp 0 8 n of
    0 -> ' '
    1 -> '▁'
    2 -> '▂'
    3 -> '▃'
    4 -> '▄'
    5 -> '▅'
    6 -> '▆'
    7 -> '▇'
    _ -> '█'

colGlyphs :: Int -> Double -> String
colGlyphs hC frac =
    let total = hC * 8
        ticks = clamp 0 total (round (frac * fromIntegral total))
        full = ticks `div` 8
        rem8 = ticks - full * 8
        topPad = hC - full - (if rem8 > 0 then 1 else 0)
        middle = [blockChar rem8 | rem8 > 0]
     in replicate topPad ' ' <> middle <> replicate full '█'

resampleToWidth :: Int -> [Double] -> [Double]
resampleToWidth w xs
    | w <= 0 = []
    | null xs = replicate w 0
    | n == w = xs
    | n > w = avgGroup (ceiling (fromIntegral n / (fromIntegral w :: Double)))
    | otherwise = replicateOut
  where
    n = length xs
    avgGroup g =
        [avg (take g (drop (i * g) xs)) | i <- [0 .. w - 1]]
      where
        avg ys = if null ys then 0 else sum ys / fromIntegral (length ys)
    replicateOut =
        let base = w `div` n
            extra = w - base * n
         in concat
                [ replicate (base + (if i < extra then 1 else 0)) v
                | (i, v) <- zip [0 ..] xs
                ]

-- | Filled-area chart with the curve closed down to @y=0@.
area :: [(Text, [(Double, Double)])] -> Plot -> Text
area sers cfg =
    renderChartTerminal $
        LC.areaChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Filled band between @(x, ymin, ymax)@ curves — e.g. CI envelopes.
ribbon :: [(Text, [(Double, Double, Double)])] -> Plot -> Text
ribbon sers cfg =
    renderChartTerminal $
        LC.ribbonChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Gaussian KDE per series (Silverman bandwidth).
density :: [(Text, [Double])] -> Plot -> Text
density sers cfg =
    renderChartTerminal $
        LC.densityChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Points with vertical error bars: @(x, y, ymin, ymax)@ per row.
errorBars :: [(Text, [(Double, Double, Double, Double)])] -> Plot -> Text
errorBars sers cfg =
    renderChartTerminal $
        LC.errorBarsChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Horizontal bars sized by their values.
funnel :: [(Text, Double)] -> Plot -> Text
funnel stages cfg =
    renderChartTerminal $
        LC.funnelChart stages (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Polar line chart; theta in radians CCW from +x.
polarLine :: [(Text, [(Double, Double)])] -> Plot -> Text
polarLine sers cfg =
    renderChartTerminal $
        LC.polarLineChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Waterfall chart: rows are @(label, start, end)@.
waterfall :: [(Text, Double, Double)] -> Plot -> Text
waterfall rows cfg =
    renderChartTerminal $
        LC.waterfallChart rows (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Histogram + KDE overlay per series.
distPlot :: [(Text, [Double])] -> Plot -> Text
distPlot sers cfg =
    renderChartTerminal $
        LC.distPlotChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)
