{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Svg
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com
Stability   : experimental
Portability : POSIX

An SVG-based plotting backend that mirrors the API of "Granite".
Every chart function returns a self-contained SVG document as 'Text'.

= Basic Usage

@
{\-# LANGUAGE OverloadedStrings #-\}
import qualified Data.Text.IO as T
import Granite.Svg

main = do
  let chart = bars [(\"Q1\",12),(\"Q2\",18),(\"Q3\",9),(\"Q4\",15)]
                   defPlot { plotTitle = \"Sales\" }
  T.writeFile \"chart.svg\" chart
@
-}
module Granite.Svg (
    -- * Re-exports from Granite
    Plot (..),
    defPlot,
    LegendPos (..),
    Color (..),
    AxisEnv (..),
    LabelFormatter,
    Bins (..),
    bins,
    series,

    -- * Chart types (SVG output)
    scatter,
    lineGraph,
    bars,
    stackedBars,
    histogram,
    pie,
    heatmap,
    boxPlot,
) where

import Data.List qualified as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Granite (
    AxisEnv (..),
    Bins (..),
    Color (..),
    LabelFormatter,
    LegendPos (..),
    Plot (..),
    bins,
    defPlot,
    series,
 )
import Numeric (showFFloat)

------------------------------------------------------------------------
-- Scaling constants
------------------------------------------------------------------------

-- | Pixels per terminal character width.
cW :: Double
cW = 10

-- | Pixels per terminal character height.
cH :: Double
cH = 16

-- | Font size for axis labels (px).
labelFontSize :: Double
labelFontSize = 11

-- | Font size for chart title (px).
titleFontSize :: Double
titleFontSize = 14

------------------------------------------------------------------------
-- Colour mapping  (ANSI → hex)
------------------------------------------------------------------------

colorHex :: Color -> Text
colorHex Default = "#555555"
colorHex Black = "#2c3e50"
colorHex Red = "#c0392b"
colorHex Green = "#27ae60"
colorHex Yellow = "#f39c12"
colorHex Blue = "#2980b9"
colorHex Magenta = "#8e44ad"
colorHex Cyan = "#16a085"
colorHex White = "#ecf0f1"
colorHex BrightBlack = "#7f8c8d"
colorHex BrightRed = "#e74c3c"
colorHex BrightGreen = "#2ecc71"
colorHex BrightYellow = "#f1c40f"
colorHex BrightBlue = "#3498db"
colorHex BrightMagenta = "#9b59b6"
colorHex BrightCyan = "#1abc9c"
colorHex BrightWhite = "#bdc3c7"

-- Heatmap intensity palette (low → high).
heatColors :: [Text]
heatColors =
    [ "#2980b9"
    , "#3498db"
    , "#16a085"
    , "#1abc9c"
    , "#27ae60"
    , "#2ecc71"
    , "#f1c40f"
    , "#f39c12"
    , "#9b59b6"
    , "#e74c3c"
    , "#c0392b"
    ]

------------------------------------------------------------------------
-- Helpers copied from Granite (not exported there)
------------------------------------------------------------------------

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

eps :: Double
eps = 1e-12

minimum', maximum' :: [Double] -> Double
minimum' [] = 0
minimum' xs = minimum xs
maximum' [] = 1
maximum' xs = maximum xs

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

quartiles :: [Double] -> (Double, Double, Double, Double, Double)
quartiles [] = (0, 0, 0, 0, 0)
quartiles xs =
    let sorted = List.sort xs
        n = length sorted
        q1Idx = n `div` 4
        q2Idx = n `div` 2
        q3Idx = 3 * n `div` 4
        getIdx i = if i < n then sorted !! i else last sorted
     in if n < 5
            then let m = sum xs / fromIntegral n in (m, m, m, m, m)
            else
                ( fromMaybe 0 (listToMaybe sorted)
                , getIdx q1Idx
                , getIdx q2Idx
                , getIdx q3Idx
                , last sorted
                )

normalize :: [(Text, Double)] -> [(Text, Double)]
normalize xs =
    let s = sum (map (abs . snd) xs) + 1e-12
     in [(n, max 0 (v / s)) | (n, v) <- xs]

ticks1D ::
    Int ->
    Int ->
    (Double, Double) ->
    Bool ->
    [(Int, Double)]
ticks1D screenLen want (vmin, vmax) invertY =
    let n = max 2 want
        lastIx = max 0 (screenLen - 1)
        toVal t =
            if invertY
                then vmax - t * (vmax - vmin)
                else vmin + t * (vmax - vmin)
        mk' k =
            let t = if n == 1 then 0 else fromIntegral k / fromIntegral (n - 1)
                pos = round (t * fromIntegral lastIx)
             in (pos, toVal t)
        raw = [mk' k | k <- [0 .. n - 1]]
     in List.nubBy (\a b -> fst a == fst b) raw

------------------------------------------------------------------------
-- Layout record
------------------------------------------------------------------------

data Layout = Layout
    { svgW :: !Double
    , svgH :: !Double
    , plotX :: !Double -- left edge of plot area
    , plotY :: !Double -- top edge of plot area
    , plotW :: !Double -- width of plot area
    , plotH :: !Double -- height of plot area
    }

mkLayout :: Plot -> Layout
mkLayout cfg =
    let pw = fromIntegral (widthChars cfg) * cW
        ph = fromIntegral (heightChars cfg) * cH
        lm = fromIntegral (leftMargin cfg) * cW + 10 -- extra padding
        tm =
            if T.null (plotTitle cfg)
                then 10
                else titleFontSize + 20
        bm = fromIntegral (bottomMargin cfg) * cH + 10
        rm = case legendPos cfg of
            LegendRight -> 120
            _ -> 20
        lb = case legendPos cfg of
            LegendBottom -> 30
            _ -> 0
     in Layout
            { svgW = lm + pw + rm
            , svgH = tm + ph + bm + lb
            , plotX = lm
            , plotY = tm
            , plotW = pw
            , plotH = ph
            }

------------------------------------------------------------------------
-- SVG primitives
------------------------------------------------------------------------

showD :: Double -> Text
showD d
    | d == fromIntegral (round d :: Int) = T.pack (show (round d :: Int))
    | otherwise = T.pack (showFFloat (Just 2) d "")

escXml :: Text -> Text
escXml =
    T.replace "&" "&amp;"
        . T.replace "<" "&lt;"
        . T.replace ">" "&gt;"
        . T.replace "\"" "&quot;"

attr :: Text -> Text -> Text
attr k v = " " <> k <> "=\"" <> v <> "\""

svgDoc :: Double -> Double -> Text -> Text
svgDoc w h content =
    "<svg xmlns=\"http://www.w3.org/2000/svg\""
        <> attr "viewBox" ("0 0 " <> showD w <> " " <> showD h)
        <> attr "width" (showD w)
        <> attr "height" (showD h)
        <> attr "font-family" "system-ui, -apple-system, sans-serif"
        <> ">\n"
        <> "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"
        <> content
        <> "</svg>\n"

svgRect :: Double -> Double -> Double -> Double -> Text -> Text -> Text
svgRect x y w h fill extra =
    "<rect"
        <> attr "x" (showD x)
        <> attr "y" (showD y)
        <> attr "width" (showD w)
        <> attr "height" (showD h)
        <> attr "fill" fill
        <> extra
        <> "/>\n"

svgCircle :: Double -> Double -> Double -> Text -> Text
svgCircle cx cy r fill =
    "<circle"
        <> attr "cx" (showD cx)
        <> attr "cy" (showD cy)
        <> attr "r" (showD r)
        <> attr "fill" fill
        <> "/>\n"

svgLine :: Double -> Double -> Double -> Double -> Text -> Double -> Text
svgLine x1 y1 x2 y2 stroke strokeW =
    "<line"
        <> attr "x1" (showD x1)
        <> attr "y1" (showD y1)
        <> attr "x2" (showD x2)
        <> attr "y2" (showD y2)
        <> attr "stroke" stroke
        <> attr "stroke-width" (showD strokeW)
        <> "/>\n"

svgPolyline :: [(Double, Double)] -> Text -> Double -> Text
svgPolyline pts stroke strokeW =
    "<polyline"
        <> attr "points" (T.intercalate " " [showD x <> "," <> showD y | (x, y) <- pts])
        <> attr "fill" "none"
        <> attr "stroke" stroke
        <> attr "stroke-width" (showD strokeW)
        <> attr "stroke-linejoin" "round"
        <> attr "stroke-linecap" "round"
        <> "/>\n"

svgText :: Double -> Double -> Text -> Text -> Double -> Text -> Text
svgText x y anchor fill size content =
    "<text"
        <> attr "x" (showD x)
        <> attr "y" (showD y)
        <> attr "text-anchor" anchor
        <> attr "fill" fill
        <> attr "font-size" (showD size)
        <> ">"
        <> escXml content
        <> "</text>\n"

svgPath :: Text -> Text -> Text -> Text
svgPath d fill extra =
    "<path"
        <> attr "d" d
        <> attr "fill" fill
        <> extra
        <> "/>\n"

------------------------------------------------------------------------
-- Shared drawing: title, axes, legend
------------------------------------------------------------------------

drawTitle :: Plot -> Layout -> Text
drawTitle cfg lay
    | T.null (plotTitle cfg) = ""
    | otherwise =
        svgText
            (plotX lay + plotW lay / 2)
            (plotY lay - 8)
            "middle"
            "#222"
            titleFontSize
            (plotTitle cfg)

drawAxes :: Plot -> Layout -> (Double, Double) -> (Double, Double) -> Text
drawAxes cfg lay (xmin, xmax) (ymin, ymax) =
    let px = plotX lay
        py = plotY lay
        pw = plotW lay
        ph = plotH lay

        -- Axis lines
        xAxis = svgLine px (py + ph) (px + pw) (py + ph) "#aaa" 1
        yAxis = svgLine px py px (py + ph) "#aaa" 1

        -- Y ticks
        yN = yNumTicks cfg
        yTks = ticks1D (round ph) yN (ymin, ymax) True
        ySlot = max 1 (leftMargin cfg)
        yEnv i = AxisEnv (ymin, ymax) i yN
        yElems =
            T.concat
                [ let frac = fromIntegral pos / max 1 (ph - 1)
                      yy = py + frac * ph
                      lbl = yFormatter cfg (yEnv i) ySlot v
                   in svgLine px yy (px - 4) yy "#aaa" 1
                        <> svgText (px - 8) (yy + 4) "end" "#555" labelFontSize lbl
                        <> svgLine px yy (px + pw) yy "#eee" 0.5 -- grid line
                | (i, (pos, v)) <- zip [0 ..] yTks
                ]

        -- X ticks
        xN = xNumTicks cfg
        xTks = ticks1D (round pw) xN (xmin, xmax) False
        xSlot = max 1 (round pw `div` max 1 xN)
        xEnv i = AxisEnv (xmin, xmax) i xN
        xElems =
            T.concat
                [ let frac = fromIntegral pos / max 1 (pw - 1)
                      xx = px + frac * pw
                      lbl = xFormatter cfg (xEnv i) xSlot v
                   in svgLine xx (py + ph) xx (py + ph + 4) "#aaa" 1
                        <> svgText xx (py + ph + 16) "middle" "#555" labelFontSize lbl
                        <> svgLine xx py xx (py + ph) "#eee" 0.5 -- grid line
                | (i, (pos, v)) <- zip [0 ..] xTks
                ]
     in xAxis <> yAxis <> yElems <> xElems

drawCatAxes :: Plot -> Layout -> (Double, Double) -> [Text] -> Text
drawCatAxes cfg lay (ymin, ymax) catNames =
    let px = plotX lay
        py = plotY lay
        pw = plotW lay
        ph = plotH lay

        xAxis = svgLine px (py + ph) (px + pw) (py + ph) "#aaa" 1
        yAxis = svgLine px py px (py + ph) "#aaa" 1

        -- Y ticks (value axis)
        yN = yNumTicks cfg
        yTks = ticks1D (round ph) yN (ymin, ymax) True
        ySlot = max 1 (leftMargin cfg)
        yEnv i = AxisEnv (ymin, ymax) i yN
        yElems =
            T.concat
                [ let frac = fromIntegral pos / max 1 (ph - 1)
                      yy = py + frac * ph
                      lbl = yFormatter cfg (yEnv i) ySlot v
                   in svgLine px yy (px - 4) yy "#aaa" 1
                        <> svgText (px - 8) (yy + 4) "end" "#555" labelFontSize lbl
                        <> svgLine px yy (px + pw) yy "#eee" 0.5
                | (i, (pos, v)) <- zip [0 ..] yTks
                ]

        -- X category labels
        n = length catNames
        catElems =
            T.concat
                [ let xx = px + (fromIntegral i + 0.5) * pw / fromIntegral n
                   in svgText xx (py + ph + 16) "middle" "#555" labelFontSize name
                | (i, name) <- zip [0 :: Int ..] catNames
                ]
     in xAxis <> yAxis <> yElems <> catElems

drawLegend :: Plot -> Layout -> [(Text, Color)] -> Text
drawLegend cfg lay entries = case legendPos cfg of
    LegendNone -> ""
    LegendRight ->
        let lx = plotX lay + plotW lay + 15
            ly = plotY lay + 5
         in T.concat
                [ svgRect lx (ly + fromIntegral i * 20) 12 12 (colorHex col) ""
                    <> svgText
                        (lx + 16)
                        (ly + fromIntegral i * 20 + 10)
                        "start"
                        "#555"
                        labelFontSize
                        name
                | (i, (name, col)) <- zip [0 :: Int ..] entries
                ]
    LegendBottom ->
        let ly = svgH lay - 18
            totalW = sum [30 + fromIntegral (T.length name) * 7 | (name, _) <- entries]
            startX = plotX lay + (plotW lay - totalW) / 2
         in snd $
                List.foldl'
                    ( \(x, acc) (name, col) ->
                        let elem' =
                                svgRect x (ly - 1) 12 12 (colorHex col) ""
                                    <> svgText (x + 16) (ly + 9) "start" "#555" labelFontSize name
                            w = 30 + fromIntegral (T.length name) * 7
                         in (x + w, acc <> elem')
                    )
                    (startX, "")
                    entries

------------------------------------------------------------------------
-- Coordinate mapping
------------------------------------------------------------------------

-- | Map data X to SVG X.
mapX :: Layout -> Double -> Double -> Double -> Double
mapX lay xmin xmax x =
    plotX lay + (x - xmin) / (xmax - xmin + eps) * plotW lay

-- | Map data Y to SVG Y (flipped: higher values → lower Y).
mapY :: Layout -> Double -> Double -> Double -> Double
mapY lay ymin ymax y =
    plotY lay + plotH lay - (y - ymin) / (ymax - ymin + eps) * plotH lay

------------------------------------------------------------------------
-- SCATTER
------------------------------------------------------------------------

scatter ::
    [(Text, [(Double, Double)])] ->
    Plot ->
    Text
scatter sers cfg =
    let lay = mkLayout cfg
        (xmin, xmax, ymin, ymax) = boundsXY cfg (concatMap snd sers)
        cols = cycle (colorPalette cfg)
        withCol = zip sers cols

        points =
            T.concat
                [ T.concat
                    [ svgCircle
                        (mapX lay xmin xmax x)
                        (mapY lay ymin ymax y)
                        3
                        (colorHex col)
                    | (x, y) <- pts
                    ]
                | ((_, pts), col) <- withCol
                ]

        axes = drawAxes cfg lay (xmin, xmax) (ymin, ymax)
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(n, col) | ((n, _), col) <- withCol]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> points <> legend)

------------------------------------------------------------------------
-- LINE GRAPH
------------------------------------------------------------------------

lineGraph ::
    [(Text, [(Double, Double)])] ->
    Plot ->
    Text
lineGraph sers cfg =
    let lay = mkLayout cfg
        (xmin, xmax, ymin, ymax) = boundsXY cfg (concatMap snd sers)
        cols = cycle (colorPalette cfg)
        withCol = zip sers cols

        lines' =
            T.concat
                [ let sortedPts = List.sortOn fst pts
                      svgPts =
                        [ (mapX lay xmin xmax x, mapY lay ymin ymax y)
                        | (x, y) <- sortedPts
                        ]
                   in svgPolyline svgPts (colorHex col) 2
                | ((_, pts), col) <- withCol
                ]

        axes = drawAxes cfg lay (xmin, xmax) (ymin, ymax)
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(n, col) | ((n, _), col) <- withCol]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> lines' <> legend)

------------------------------------------------------------------------
-- BARS
------------------------------------------------------------------------

bars ::
    [(Text, Double)] ->
    Plot ->
    Text
bars kvs cfg =
    let lay = mkLayout cfg
        (catNames, vals) = unzip kvs
        vmax = maximum' (map abs vals)
        cols = cycle (colorPalette cfg)
        n = length kvs

        barGap = 0.15 -- 15% gap on each side
        groupW = plotW lay / fromIntegral (max 1 n)

        rects =
            T.concat
                [ let barW = groupW * (1 - 2 * barGap)
                      barH = abs v / (vmax + eps) * plotH lay
                      bx = plotX lay + fromIntegral i * groupW + groupW * barGap
                      by = plotY lay + plotH lay - barH
                   in svgRect bx by barW barH (colorHex col) (attr "rx" "2")
                | (i, (_, v), col) <- zip3 [0 :: Int ..] kvs cols
                ]

        axes = drawCatAxes cfg lay (0, vmax) catNames
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(name, col) | ((name, _), col) <- zip kvs cols]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> rects <> legend)

------------------------------------------------------------------------
-- STACKED BARS
------------------------------------------------------------------------

stackedBars ::
    [(Text, [(Text, Double)])] ->
    Plot ->
    Text
stackedBars categories cfg =
    let lay = mkLayout cfg
        seriesNames = case categories of
            [] -> []
            (c : _) -> map fst (snd c)

        totals = [sum (map snd s) | (_, s) <- categories]
        maxH = maximum (1e-12 : totals)

        cols = cycle (colorPalette cfg)
        seriesColors = zip seriesNames cols

        nCats = length categories
        barGap = 0.1
        groupW = plotW lay / fromIntegral (max 1 nCats)

        rects =
            T.concat
                [ let barW = groupW * (1 - 2 * barGap)
                      bx = plotX lay + fromIntegral ci * groupW + groupW * barGap
                      cumHeights = scanl (+) 0 [v / maxH | (_, v) <- segs]
                   in T.concat
                        [ let bot = cumHeights !! si
                              top = cumHeights !! (si + 1)
                              segH = (top - bot) * plotH lay
                              segY = plotY lay + plotH lay - top * plotH lay
                              col = fromMaybe BrightBlue (lookup sn seriesColors)
                           in svgRect bx segY barW segH (colorHex col) (attr "rx" "1")
                        | (si, (sn, _)) <- zip [0 :: Int ..] segs
                        ]
                | (ci, (_, segs)) <- zip [0 :: Int ..] categories
                ]

        catNames = map fst categories
        axes = drawCatAxes cfg lay (0, maxH) catNames
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(n, col) | (n, col) <- seriesColors]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> rects <> legend)

------------------------------------------------------------------------
-- HISTOGRAM
------------------------------------------------------------------------

histogram ::
    Bins ->
    [Double] ->
    Plot ->
    Text
histogram (Bins nB a b) xs cfg =
    let lay = mkLayout cfg
        step = (b - a) / fromIntegral nB
        binIx x = clamp 0 (nB - 1) $ floor ((x - a) / step)
        counts0 :: [Int]
        counts0 =
            List.foldl'
                ( \acc x ->
                    if x < a || x > b
                        then acc
                        else addAt acc (binIx x) 1
                )
                (replicate nB 0)
                xs
        maxC = fromIntegral (maximum (1 : counts0)) :: Double

        barW = plotW lay / fromIntegral nB
        rects =
            T.concat
                [ let v = fromIntegral c
                      barH = v / (maxC + eps) * plotH lay
                      bx = plotX lay + fromIntegral i * barW
                      by = plotY lay + plotH lay - barH
                   in svgRect bx by (barW - 1) barH (colorHex BrightCyan) ""
                | (i, c) <- zip [0 :: Int ..] counts0
                ]

        axes = drawAxes cfg lay (a, b) (0, maxC)
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [("count", BrightCyan)]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> rects <> legend)

------------------------------------------------------------------------
-- PIE
------------------------------------------------------------------------

pie ::
    [(Text, Double)] ->
    Plot ->
    Text
pie parts0 cfg =
    let lay = mkLayout cfg
        parts = normalize parts0

        cxP = plotX lay + plotW lay / 2
        cyP = plotY lay + plotH lay / 2
        r = min (plotW lay / 2) (plotH lay / 2) * 0.85

        toAng p = p * 2 * pi
        wedgeAngles = scanl (\acc (_, p) -> acc + toAng p) 0 parts
        angles = zip wedgeAngles (drop 1 wedgeAngles)

        cols = cycle (colorPalette cfg)
        withP = zip3 (map fst parts) angles cols

        slices =
            T.concat
                [ let (a0, a1) = ang
                      x0 = cxP + r * cos a0
                      y0 = cyP - r * sin a0
                      x1 = cxP + r * cos a1
                      y1 = cyP - r * sin a1
                      largeArc = if a1 - a0 > pi then "1" else "0"
                      d =
                        "M "
                            <> showD cxP
                            <> " "
                            <> showD cyP
                            <> " L "
                            <> showD x0
                            <> " "
                            <> showD y0
                            <> " A "
                            <> showD r
                            <> " "
                            <> showD r
                            <> " 0 "
                            <> largeArc
                            <> " 0 "
                            <> showD x1
                            <> " "
                            <> showD y1
                            <> " Z"
                   in svgPath d (colorHex col) (attr "stroke" "white" <> attr "stroke-width" "2")
                | (_, ang, col) <- withP
                ]

        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(n, col) | (n, _, col) <- withP]
     in svgDoc (svgW lay) (svgH lay) (title <> slices <> legend)

------------------------------------------------------------------------
-- HEATMAP
------------------------------------------------------------------------

heatmap ::
    [[Double]] ->
    Plot ->
    Text
heatmap matrix cfg =
    let lay = mkLayout cfg
        rows = length matrix
        cols = case matrix of
            [] -> 0
            (r : _) -> length r

        allVals = concat matrix
        vmin = if null allVals then 0 else minimum' allVals
        vmax = if null allVals then 1 else maximum' allVals
        vrange = vmax - vmin

        nColors = length heatColors
        colorForVal v =
            if vrange < eps
                then heatColors !! (nColors `div` 2)
                else
                    let norm = clamp 0 1 ((v - vmin) / vrange)
                        idx = clamp 0 (nColors - 1) (floor (norm * fromIntegral (nColors - 1)))
                     in heatColors !! idx

        cellW = plotW lay / fromIntegral (max 1 cols)
        cellH = plotH lay / fromIntegral (max 1 rows)

        cells =
            T.concat
                [ T.concat
                    [ let cx = plotX lay + fromIntegral c * cellW
                          cy = plotY lay + fromIntegral r * cellH
                          dataRow = rows - 1 - r -- flip: row 0 at bottom
                          v = (matrix !! dataRow) !! c
                       in svgRect cx cy cellW cellH (colorForVal v) ""
                    | c <- [0 .. cols - 1]
                    ]
                | r <- [0 .. rows - 1]
                ]

        -- Axis labels: column indices on x, row indices on y
        colLabels =
            T.concat
                [ svgText
                    (plotX lay + (fromIntegral c + 0.5) * cellW)
                    (plotY lay + plotH lay + 16)
                    "middle"
                    "#555"
                    labelFontSize
                    (T.pack (show c))
                | c <- [0 .. cols - 1]
                ]
        rowLabels =
            T.concat
                [ svgText
                    (plotX lay - 8)
                    (plotY lay + (fromIntegral r + 0.5) * cellH + 4)
                    "end"
                    "#555"
                    labelFontSize
                    (T.pack (show (rows - 1 - r)))
                | r <- [0 .. rows - 1]
                ]

        -- Draw border
        border =
            svgRect
                (plotX lay)
                (plotY lay)
                (plotW lay)
                (plotH lay)
                "none"
                (attr "stroke" "#aaa" <> attr "stroke-width" "1")

        title = drawTitle cfg lay

        -- Gradient legend
        gradLegend =
            let gw = min 200 (plotW lay * 0.5)
                gh = 12
                gx = plotX lay + (plotW lay - gw) / 2
                gy = svgH lay - 22
                nSteps = 20 :: Int
                stepW = gw / fromIntegral nSteps
             in svgText
                    (gx - 5)
                    (gy + 10)
                    "end"
                    "#555"
                    labelFontSize
                    (T.pack (showFFloat (Just 2) vmin ""))
                    <> T.concat
                        [ let t = fromIntegral i / fromIntegral nSteps
                              v = vmin + t * vrange
                           in svgRect (gx + fromIntegral i * stepW) gy stepW gh (colorForVal v) ""
                        | i <- [0 .. nSteps - 1]
                        ]
                    <> svgText
                        (gx + gw + 5)
                        (gy + 10)
                        "start"
                        "#555"
                        labelFontSize
                        (T.pack (showFFloat (Just 2) vmax ""))
     in svgDoc
            (svgW lay)
            (svgH lay)
            (title <> cells <> colLabels <> rowLabels <> border <> gradLegend)

------------------------------------------------------------------------
-- BOX PLOT
------------------------------------------------------------------------

boxPlot ::
    [(Text, [Double])] ->
    Plot ->
    Text
boxPlot datasets cfg =
    let lay = mkLayout cfg
        stats = [(name, quartiles vals) | (name, vals) <- datasets]

        allVals = concatMap snd datasets
        ymin = if null allVals then 0 else minimum' allVals - abs (minimum' allVals) * 0.1
        ymax = if null allVals then 1 else maximum' allVals + abs (maximum' allVals) * 0.1

        nBoxes = length datasets
        cols = cycle (colorPalette cfg)

        groupW = plotW lay / fromIntegral (max 1 nBoxes)
        boxW = groupW * 0.6

        scY v = plotY lay + plotH lay - (v - ymin) / (ymax - ymin + eps) * plotH lay

        boxes =
            T.concat
                [ let bx = plotX lay + fromIntegral i * groupW + (groupW - boxW) / 2
                      midX = bx + boxW / 2
                      (minV, q1, med, q3, maxV) = qs
                      col = colorHex c

                      -- Whisker: min to Q1
                      whiskerBot = svgLine midX (scY minV) midX (scY q1) col 1.5
                      capBot = svgLine (bx + boxW * 0.25) (scY minV) (bx + boxW * 0.75) (scY minV) col 1.5

                      -- Box: Q1 to Q3
                      boxY = scY q3
                      boxH = scY q1 - scY q3
                      box =
                        svgRect
                            bx
                            boxY
                            boxW
                            boxH
                            col
                            ( attr "fill-opacity" "0.3"
                                <> attr "stroke" col
                                <> attr "stroke-width" "1.5"
                                <> attr "rx" "2"
                            )

                      -- Median line
                      medLine = svgLine bx (scY med) (bx + boxW) (scY med) col 2.5

                      -- Whisker: Q3 to max
                      whiskerTop = svgLine midX (scY q3) midX (scY maxV) col 1.5
                      capTop = svgLine (bx + boxW * 0.25) (scY maxV) (bx + boxW * 0.75) (scY maxV) col 1.5
                   in whiskerBot <> capBot <> whiskerTop <> capTop <> box <> medLine
                | (i, (_, qs), c) <- zip3 [0 :: Int ..] stats cols
                ]

        catNames = map fst datasets
        axes = drawCatAxes cfg lay (ymin, ymax) catNames
        title = drawTitle cfg lay
        legend = drawLegend cfg lay [(name, col) | ((name, _), col) <- zip stats cols]
     in svgDoc (svgW lay) (svgH lay) (title <> axes <> boxes <> legend)

addAt :: [Int] -> Int -> Int -> [Int]
addAt xs i v = updateAt xs i (+ v)

updateAt :: [a] -> Int -> (a -> a) -> [a]
updateAt xs i f
    | i < 0 = xs
    | otherwise = go xs i
  where
    go [] _ = []
    go (x : rest) 0 = f x : rest
    go (x : rest) n = x : go rest (n - 1)
