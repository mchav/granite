{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Svg
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

SVG plotting backend that mirrors the API of "Granite". Each chart
function returns a self-contained SVG document as 'Text'.
-}
module Granite.Svg (
    Plot (..),
    defPlot,
    LegendPos (..),
    Color (..),
    AxisEnv (..),
    LabelFormatter,
    Bins (..),
    bins,
    series,
    scatter,
    lineGraph,
    bars,
    stackedBars,
    histogram,
    pie,
    heatmap,
    boxPlot,
    area,
    ribbon,
    density,
    errorBars,
    funnel,
    polarLine,
    waterfall,
    distPlot,
    gauss,
) where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
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
import Granite.Color (colorHex)
import Granite.Internal.LegacyChart qualified as LC
import Granite.Internal.Util (
    addAt,
    clamp,
    eps,
    maximum',
    minimum',
    normalize,
    quartiles,
    showD,
    ticks1D,
 )
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Render.Svg (
    attr,
    svgCircle,
    svgDoc,
    svgLine,
    svgPath,
    svgPolyline,
    svgRect,
    svgText,
 )
import Numeric (showFFloat)

cW :: Double
cW = 10

cH :: Double
cH = 16

labelFontSize :: Double
labelFontSize = 11

titleFontSize :: Double
titleFontSize = 14

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

data Layout = Layout
    { svgW :: !Double
    , svgH :: !Double
    , plotX :: !Double
    , plotY :: !Double
    , plotW :: !Double
    , plotH :: !Double
    }

mkLayout :: Plot -> Layout
mkLayout cfg =
    let pw = fromIntegral (widthChars cfg) * cW
        ph = fromIntegral (heightChars cfg) * cH
        lm = fromIntegral (leftMargin cfg) * cW + 10
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

        xAxis = svgLine px (py + ph) (px + pw) (py + ph) "#aaa" 1
        yAxis = svgLine px py px (py + ph) "#aaa" 1

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
                        <> svgLine xx py xx (py + ph) "#eee" 0.5
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

mapX :: Layout -> Double -> Double -> Double -> Double
mapX lay xmin xmax x =
    plotX lay + (x - xmin) / (xmax - xmin + eps) * plotW lay

mapY :: Layout -> Double -> Double -> Double -> Double
mapY lay ymin ymax y =
    plotY lay + plotH lay - (y - ymin) / (ymax - ymin + eps) * plotH lay

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

        barGap = 0.15
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
                          dataRow = rows - 1 - r
                          v = (matrix !! dataRow) !! c
                       in svgRect cx cy cellW cellH (colorForVal v) ""
                    | c <- [0 .. cols - 1]
                    ]
                | r <- [0 .. rows - 1]
                ]

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

        border =
            svgRect
                (plotX lay)
                (plotY lay)
                (plotW lay)
                (plotH lay)
                "none"
                (attr "stroke" "#aaa" <> attr "stroke-width" "1")

        title = drawTitle cfg lay

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

                      whiskerBot = svgLine midX (scY minV) midX (scY q1) col 1.5
                      capBot = svgLine (bx + boxW * 0.25) (scY minV) (bx + boxW * 0.75) (scY minV) col 1.5

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

                      medLine = svgLine bx (scY med) (bx + boxW) (scY med) col 2.5

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

area :: [(Text, [(Double, Double)])] -> Plot -> Text
area sers cfg =
    renderChartSvg $
        LC.areaChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

ribbon :: [(Text, [(Double, Double, Double)])] -> Plot -> Text
ribbon sers cfg =
    renderChartSvg $
        LC.ribbonChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

density :: [(Text, [Double])] -> Plot -> Text
density sers cfg =
    renderChartSvg $
        LC.densityChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

errorBars :: [(Text, [(Double, Double, Double, Double)])] -> Plot -> Text
errorBars sers cfg =
    renderChartSvg $
        LC.errorBarsChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

funnel :: [(Text, Double)] -> Plot -> Text
funnel stages cfg =
    renderChartSvg $
        LC.funnelChart stages (widthChars cfg) (heightChars cfg) (plotTitle cfg)

polarLine :: [(Text, [(Double, Double)])] -> Plot -> Text
polarLine sers cfg =
    renderChartSvg $
        LC.polarLineChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

waterfall :: [(Text, Double, Double)] -> Plot -> Text
waterfall rows cfg =
    renderChartSvg $
        LC.waterfallChart rows (widthChars cfg) (heightChars cfg) (plotTitle cfg)

distPlot :: [(Text, [Double])] -> Plot -> Text
distPlot sers cfg =
    renderChartSvg $
        LC.distPlotChart sers (widthChars cfg) (heightChars cfg) (plotTitle cfg)

-- | Gaussian / normal-distribution chart in standard-deviation (z-score) units.
-- Mirrors 'Granite.gauss': a KDE bell curve with a stippled scatter fill, an
-- x-axis labelled in σ units, and lollipop annotations for named markers
-- (the largest z highlighted as the outlier).
gauss ::
    -- | Population sample (used to compute μ and σ)
    [Double] ->
    -- | Named markers as @(label, raw value)@; the largest z is highlighted
    [(Text, Double)] ->
    Plot ->
    Text
gauss population markers cfg =
    let n = length population
        mu = if n == 0 then 0 else sum population / fromIntegral n
        var =
            if n < 2
                then 0
                else sum [(x - mu) ^ (2 :: Int) | x <- population] / fromIntegral (n - 1)
        sigma = sqrt (max var eps)
        z v = (v - mu) / sigma

        zsData = map z population
        zsMark = [(name, z v) | (name, v) <- markers]
        maxMarkerZ = maximum' (0 : [zz | (_, zz) <- zsMark])

        allZ = zsData <> [zz | (_, zz) <- zsMark]
        zlo0 = minimum' (0 : allZ)
        zhi0 = maximum' (1 : allZ)
        padz = (zhi0 - zlo0) * 0.06 + 1e-9
        zmin = zlo0 - padz
        zmax = zhi0 + padz

        -- Gaussian KDE in z-space (Silverman bandwidth).
        sdz =
            let m = sum zsData / fromIntegral (max 1 n)
             in sqrt (max eps (sum [(zz - m) ^ (2 :: Int) | zz <- zsData] / fromIntegral (max 1 (n - 1))))
        hbw = max 1e-6 (1.06 * sdz * fromIntegral (max 1 n) ** (-0.2))
        dens x =
            sum [exp (negate ((x - zz) ^ (2 :: Int)) / (2 * hbw * hbw)) | zz <- zsData]
                / (fromIntegral (max 1 n) * hbw * sqrt (2 * pi))

        lay = mkLayout cfg
        baseY = plotY lay + plotH lay
        mx = mapX lay zmin zmax

        nGrid = max 8 (widthChars cfg * 3)
        zAt i = zmin + fromIntegral i / fromIntegral nGrid * (zmax - zmin)
        densVals = [dens (zAt i) | i <- [0 .. nGrid]]
        dmax = maximum' densVals + eps
        topFrac = 0.9
        densToY d = baseY - (d / dmax) * topFrac * plotH lay

        curvePts = [(mx (zAt i), densToY d) | (i, d) <- zip [0 :: Int ..] densVals]

        -- Light fill under the curve.
        fillPath =
            svgPath
                ( "M "
                    <> showD (fst (head curvePts))
                    <> " "
                    <> showD baseY
                    <> " "
                    <> T.concat ["L " <> showD x <> " " <> showD y <> " " | (x, y) <- curvePts]
                    <> "L "
                    <> showD (fst (last curvePts))
                    <> " "
                    <> showD baseY
                    <> " Z"
                )
                "#dfe4e8"
                (attr "fill-opacity" "0.6")
        curveStroke = svgPolyline curvePts "#5b6770" 2

        -- Faint dashed guide at the mean (z = 0).
        meanLine
            | zmin <= 0 && zmax >= 0 =
                "<line"
                    <> attr "x1" (showD (mx 0))
                    <> attr "y1" (showD (plotY lay))
                    <> attr "x2" (showD (mx 0))
                    <> attr "y2" (showD baseY)
                    <> attr "stroke" "#cfd6dc"
                    <> attr "stroke-width" "1"
                    <> attr "stroke-dasharray" "3 4"
                    <> "/>\n"
            | otherwise = ""

        -- Deterministic scatter fill, dot count per column ∝ curve height.
        nx = max 4 (widthChars cfg)
        cellPx = 14 :: Double
        hashI a = let b = a * 1103515245 + 12345 in b * 1103515245 + 12345
        frac k = fromIntegral (abs (hashI k) `mod` 1000) / 1000
        dots =
            T.concat
                [ let zc = zmin + (fromIntegral gx + 0.5) / fromIntegral nx * (zmax - zmin)
                      cxc = mx zc
                      yTop = densToY (dens zc)
                      colHpx = baseY - yTop
                      rowsHere = max 0 (floor (colHpx / cellPx)) :: Int
                   in T.concat
                        [ let kk = gx * 1000 + gy
                           in if abs (hashI (kk * 31 + 7)) `mod` 100 >= 78
                                then ""
                                else
                                    let jx = (frac (kk * 3 + 1) - 0.5) * (plotW lay / fromIntegral nx)
                                        jy = (frac (kk * 5 + 2) - 0.5) * cellPx
                                        dx = cxc + jx
                                        dy = yTop + (fromIntegral gy + 0.5) / fromIntegral (max 1 rowsHere) * colHpx + jy
                                     in svgCircle dx dy 1.3 "#aeb4bb"
                        | gy <- [0 .. rowsHere - 1]
                        ]
                | gx <- [0 .. nx - 1]
                ]

        -- σ x-axis: a tick for each integer standard deviation in range.
        xBaseline = svgLine (plotX lay) baseY (plotX lay + plotW lay) baseY "#aaa" 1
        sigInts = [k | k <- [ceiling zmin .. floor zmax :: Int], k /= 0]
        xticks =
            T.concat
                [ let xx = mx (fromIntegral k)
                   in svgLine xx baseY xx (baseY + 5) "#aaa" 1
                        <> svgText xx (baseY + 18) "middle" "#777" labelFontSize (T.pack (show k) <> "σ")
                | k <- sigInts
                ]
        avgLbl
            | zmin <= 0 && zmax >= 0 =
                svgText (mx 0) (baseY + 18) "middle" "#999" labelFontSize "average"
                    <> svgText (mx 0) (baseY + 32) "middle" "#bbb" (labelFontSize - 1) (T.pack (showFFloat (Just 2) mu ""))
            | otherwise = ""

        -- Lollipop annotations, stacked into lanes so labels never overlap.
        sigmaTxt zz = T.pack (showFFloat (Just 1) zz "") <> "σ"
        lblOf (name, zz) = name <> " " <> sigmaTxt zz
        wpx s = fromIntegral (T.length s) * 6.2
        sortedM = List.sortOn (\(_, zz) -> mx zz) zsMark
        updLane occ i v =
            let need = i + 1 - length occ
                occ' = if need > 0 then occ <> replicate need (-1 / 0) else occ
             in [if j == i then v else e | (j, e) <- zip [0 ..] occ']
        place _ [] = []
        place occ ((name, zz) : rest) =
            let s = lblOf (name, zz)
                cx = mx zz
                start = cx - wpx s / 2
                pick es i = case es of
                    [] -> i
                    (e : es') -> if start > e + 6 then i else pick es' (i + 1)
                row = pick occ 0
             in (row, name, zz, s) : place (updLane occ row (cx + wpx s / 2)) rest
        placed = place [] sortedM

        anchorFor cx
            | cx > plotX lay + plotW lay * 0.82 = "end"
            | cx < plotX lay + plotW lay * 0.18 = "start"
            | otherwise = "middle"

        markerEls =
            T.concat
                [ let cx = mx zz
                      isHi = zz >= maxMarkerZ - eps
                      col = if isHi then "#ff2e88" else "#3a86c8"
                      labelY = plotY lay + 12 + fromIntegral row * 15
                      fontSz = if isHi then labelFontSize + 2 else labelFontSize
                   in svgLine cx baseY cx (labelY + 4) col (if isHi then 1.8 else 1)
                        <> svgCircle cx baseY (if isHi then 6 else 3) col
                        <> svgText cx labelY (anchorFor cx) col fontSz s
                | (row, _name, zz, s) <- placed
                ]

        title = drawTitle cfg lay
     in svgDoc
            (svgW lay)
            (svgH lay)
            (title <> fillPath <> meanLine <> dots <> curveStroke <> xBaseline <> xticks <> avgLbl <> markerEls)
