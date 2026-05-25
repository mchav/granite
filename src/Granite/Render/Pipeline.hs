{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Render.Pipeline
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Chart → Scene pipeline. 'chartToScene' compiles a 'Chart' spec into
a backend-agnostic 'Scene'; 'renderChartTerminal' and
'renderChartSvg' pair it with the matching backend.
-}
module Granite.Render.Pipeline (
    renderChart,
    renderChartTerminal,
    renderChartSvg,
    chartToScene,
) where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import Granite.Color (Color (..), parseHex)
import Granite.Data.Frame (
    Column (..),
    DataFrame (..),
    columnAsNum,
    columnAsText,
    filterByRows,
    lookupColumn,
 )
import Granite.Position (applyPosition)
import Granite.Render.Chrome (
    PlotBox (..),
    axesMarks,
    computePlotBox,
    legendMarks,
    titleMarks,
 )
import Granite.Render.Scene (
    Mark (..),
    Point (..),
    Rect (..),
    Scene (..),
    Style (..),
    TextAnchor (..),
    TextStyle (..),
    defaultStyle,
    defaultTextStyle,
 )
import Granite.Render.Svg qualified as Svg
import Granite.Render.Terminal qualified as Terminal
import Granite.Scale (TrainedScale (..), train)
import Granite.Spec (
    AesDefaults (..),
    Chart (..),
    ColorSpec (..),
    ColumnRef (..),
    Coord (..),
    Facet (..),
    FacetScales (..),
    Geom (..),
    Layer (..),
    Mapping (..),
    PolarAes (..),
    PolarDir (..),
    Scales (..),
    Size (..),
    Theme (..),
    aesX,
    aesY,
 )
import Granite.Stat (applyStat)

type Projector = Double -> Double -> Point

buildProjector :: Coord -> PlotBox -> TrainedScale -> TrainedScale -> Projector
buildProjector c box xs ys = case c of
    CoordCartesian -> cartesianProjector box xs ys
    CoordFlip -> flipProjector box xs ys
    CoordPolar aes a0 dir -> polarProjector aes a0 dir box xs ys

cartesianProjector :: PlotBox -> TrainedScale -> TrainedScale -> Projector
cartesianProjector box xs ys x y =
    Point
        (boxX box + tsProject xs x * boxW box)
        (boxY box + boxH box - tsProject ys y * boxH box)

{- | 'CoordFlip' rotates the chart 90° clockwise: the X aesthetic
projects top-down (data x=0 → top), matching ggplot @coord_flip()@
and Plotly's horizontal-bar / funnel convention.
-}
flipProjector :: PlotBox -> TrainedScale -> TrainedScale -> Projector
flipProjector box xs ys x y =
    Point
        (boxX box + tsProject ys y * boxW box)
        (boxY box + tsProject xs x * boxH box)

polarProjector ::
    PolarAes ->
    Double ->
    PolarDir ->
    PlotBox ->
    TrainedScale ->
    TrainedScale ->
    Projector
polarProjector aes a0 dir box xs ys =
    let cx = boxX box + boxW box / 2
        cy = boxY box + boxH box / 2
        maxR = min (boxW box) (boxH box) / 2
        sign = case dir of
            PolarCW -> 1
            PolarCCW -> -1
     in \x y ->
            let (tVal, rVal) = case aes of
                    ThetaX -> (tsProject xs x, tsProject ys y)
                    ThetaY -> (tsProject ys y, tsProject xs x)
                theta = a0 + sign * tVal * 2 * pi
                r = rVal * maxR
             in Point (cx + r * cos theta) (cy + r * sin theta)

renderChartTerminal :: Chart -> Text
renderChartTerminal = Terminal.renderScene . chartToScene

{- | SVG render; uses responsive sizing when 'chartSize' is
'SizeResponsive', explicit pixel dimensions otherwise.
-}
renderChartSvg :: Chart -> Text
renderChartSvg chart =
    let render = case chartSize chart of
            SizeResponsive _ -> Svg.renderSceneResponsive
            _ -> Svg.renderScene
     in render (chartToScene chart)

renderChart :: Chart -> (Text, Text)
renderChart c = (renderChartTerminal c, renderChartSvg c)

chartToScene :: Chart -> Scene
chartToScene chart =
    let theme = chartTheme chart
        palette = themePalette theme
        coord = chartCoord chart
        hasTitle = case chartTitle chart of
            Just t -> not (Text.null t)
            Nothing -> False
        colorMaps = map (layerColorMap palette (chartData chart)) (chartLayers chart)
        legendEntries = collectLegend (chartLayers chart) palette colorMaps
        hasRightLegend = not (null legendEntries)
        box = computePlotBox (chartSize chart) theme hasTitle hasRightLegend False

        panels = layoutPanels theme box chart
        panelMarks = concatMap (renderPanel theme palette coord colorMaps) panels

        legend = legendMarks theme box legendEntries
        title = titleMarks theme box (chartTitle chart)

        marks = panelMarks <> title <> legend
     in Scene (boxSceneW box) (boxSceneH box) marks

data PanelSpec = PanelSpec
    { panelBox :: PlotBox
    , panelLabel :: Text
    , panelData :: DataFrame
    , panelLayers :: [Layer]
    , panelXScale :: TrainedScale
    , panelYScale :: TrainedScale
    }

renderPanel ::
    Theme ->
    [ColorSpec] ->
    Coord ->
    [Maybe [(Text, ColorSpec)]] ->
    PanelSpec ->
    [Mark]
renderPanel theme palette coord colorMaps ps =
    let proj = buildProjector coord (panelBox ps) (panelXScale ps) (panelYScale ps)
        cmap i = if i < length colorMaps then colorMaps !! i else Nothing
        layerMarks =
            concat
                [ runLayer theme (panelBox ps) proj palette (cmap i) i (panelData ps) layer
                | (i, layer) <- zip [0 :: Int ..] (panelLayers ps)
                ]
        axes = axesMarks theme (panelBox ps) coord (panelXScale ps) (panelYScale ps)
        strip = stripMark theme (panelBox ps) (panelLabel ps)
     in axes <> layerMarks <> strip

stripMark :: Theme -> PlotBox -> Text -> [Mark]
stripMark theme box label
    | Text.null label = []
    | otherwise =
        [ MText
            (Point (boxX box + boxW box / 2) (boxY box - 4))
            label
            defaultTextStyle
                { textFill = colorOfSpec (themeTextColor theme)
                , textSize = themeFontSize theme
                , textAnchor = AnchorMiddle
                }
        ]
  where
    colorOfSpec spec = case spec of
        NamedColor c -> c
        _ -> Default

layoutPanels :: Theme -> PlotBox -> Chart -> [PanelSpec]
layoutPanels theme box chart = case chartFacet chart of
    FacetNull ->
        [singlePanel chart box ""]
    FacetWrap colRef mNcol mNrow scalesMode ->
        let keys = facetKeys colRef (chartData chart) (chartLayers chart)
         in case keys of
                [] -> [singlePanel chart box ""]
                _ ->
                    let n = length keys
                        (nrow, ncol) = wrapDims n mNcol mNrow
                        cells = gridCells theme box nrow ncol
                     in [ panelFor chart colRef key scalesMode cellBox
                        | (key, cellBox) <- zip keys cells
                        ]
    FacetGrid rowRefs colRefs scalesMode ->
        let (rowKeys, colKeys) = gridKeys rowRefs colRefs (chartData chart) (chartLayers chart)
            rowKeys' = if null rowKeys then [""] else rowKeys
            colKeys' = if null colKeys then [""] else colKeys
            nrow = length rowKeys'
            ncol = length colKeys'
            cells = gridCells theme box nrow ncol
         in [ gridPanel chart rowRefs colRefs rowKey colKey scalesMode cellBox
            | (rowKey, rowIx) <- zip rowKeys' [0 :: Int ..]
            , (colKey, colIx) <- zip colKeys' [0 :: Int ..]
            , let cellBox = cells !! (rowIx * ncol + colIx)
            ]

singlePanel :: Chart -> PlotBox -> Text -> PanelSpec
singlePanel chart box label =
    let xRange = paddedRange (chartLayers chart) (chartData chart) xRangeRefs True
        yRange = paddedRange (chartLayers chart) (chartData chart) yRangeRefs False
        xs0 = train (scaleX (chartScales chart)) xRange
        ys0 = train (scaleY (chartScales chart)) yRange
        xs = applyCategorical aesX (chartLayers chart) (chartData chart) xs0
        ys = applyCategorical aesY (chartLayers chart) (chartData chart) ys0
     in PanelSpec
            { panelBox = box
            , panelLabel = label
            , panelData = chartData chart
            , panelLayers = chartLayers chart
            , panelXScale = xs
            , panelYScale = ys
            }

panelFor :: Chart -> ColumnRef -> Text -> FacetScales -> PlotBox -> PanelSpec
panelFor chart colRef key scalesMode box =
    let (frame, layers) = filterChartByKey chart colRef key
        chart' = chart{chartData = frame, chartLayers = layers}
        (xs, ys) = trainPanelScales chart chart' scalesMode
     in PanelSpec
            { panelBox = box
            , panelLabel = key
            , panelData = frame
            , panelLayers = layers
            , panelXScale = xs
            , panelYScale = ys
            }

gridPanel ::
    Chart ->
    [ColumnRef] ->
    [ColumnRef] ->
    Text ->
    Text ->
    FacetScales ->
    PlotBox ->
    PanelSpec
gridPanel chart rowRefs colRefs rowKey colKey scalesMode box =
    let frame0 = chartData chart
        frame1 = foldl (\f c -> filterFrameByKey c rowKey f) frame0 rowRefs
        frame2 = foldl (\f c -> filterFrameByKey c colKey f) frame1 colRefs
        filterLayer l =
            l
                { layerData =
                    fmap
                        ( \df ->
                            let g0 = foldl (\f c -> filterFrameByKey c rowKey f) df rowRefs
                             in foldl (\f c -> filterFrameByKey c colKey f) g0 colRefs
                        )
                        (layerData l)
                }
        layers = map filterLayer (chartLayers chart)
        chart' = chart{chartData = frame2, chartLayers = layers}
        label = case (Text.null rowKey, Text.null colKey) of
            (True, True) -> ""
            (True, False) -> colKey
            (False, True) -> rowKey
            (False, False) -> rowKey <> " | " <> colKey
        (xs, ys) = trainPanelScales chart chart' scalesMode
     in PanelSpec
            { panelBox = box
            , panelLabel = label
            , panelData = frame2
            , panelLayers = layers
            , panelXScale = xs
            , panelYScale = ys
            }

trainPanelScales ::
    Chart -> Chart -> FacetScales -> (TrainedScale, TrainedScale)
trainPanelScales whole panel mode =
    let xS = scaleX (chartScales whole)
        yS = scaleY (chartScales whole)
        wholeXRange = paddedRange (chartLayers whole) (chartData whole) xRangeRefs True
        wholeYRange = paddedRange (chartLayers whole) (chartData whole) yRangeRefs False
        panelXRange = paddedRange (chartLayers panel) (chartData panel) xRangeRefs True
        panelYRange = paddedRange (chartLayers panel) (chartData panel) yRangeRefs False
        (xRange, yRange) = case mode of
            ScalesFixed -> (wholeXRange, wholeYRange)
            ScalesFreeX -> (panelXRange, wholeYRange)
            ScalesFreeY -> (wholeXRange, panelYRange)
            ScalesFree -> (panelXRange, panelYRange)
        xs = applyCategorical aesX (chartLayers panel) (chartData panel) (train xS xRange)
        ys = applyCategorical aesY (chartLayers panel) (chartData panel) (train yS yRange)
     in (xs, ys)

{- | Strip height of 32 px = two terminal lines, so panel data never
shares a character row with its facet strip label.
-}
gridCells :: Theme -> PlotBox -> Int -> Int -> [PlotBox]
gridCells theme box nrow ncol =
    let stripH = max 32 (themeFontSize theme * 2 + 10)
        gutterX = 10
        gutterY = 14
        cellW = boxW box / fromIntegral ncol
        cellH = boxH box / fromIntegral nrow
     in [ box
            { boxX = boxX box + fromIntegral c * cellW + gutterX / 2
            , boxY = boxY box + fromIntegral r * cellH + stripH
            , boxW = cellW - gutterX
            , boxH = cellH - stripH - gutterY
            }
        | r <- [0 .. nrow - 1]
        , c <- [0 .. ncol - 1]
        ]

wrapDims :: Int -> Maybe Int -> Maybe Int -> (Int, Int)
wrapDims n mNcol mNrow = case (mNcol, mNrow) of
    (Just c, Just r) -> (max 1 r, max 1 c)
    (Just c, Nothing) -> (ceilingDiv n (max 1 c), max 1 c)
    (Nothing, Just r) -> (max 1 r, ceilingDiv n (max 1 r))
    (Nothing, Nothing) ->
        let c = max 1 (ceiling (sqrt (fromIntegral n :: Double)))
         in (ceilingDiv n c, c)
  where
    ceilingDiv a b = (a + b - 1) `div` b

facetKeys :: ColumnRef -> DataFrame -> [Layer] -> [Text]
facetKeys colRef chartFrame layers =
    case keysFrom colRef chartFrame of
        Just ks -> List.nub ks
        Nothing ->
            List.nub $
                concat
                    [ ks
                    | Just frame <- map layerData layers
                    , Just ks <- [keysFrom colRef frame]
                    ]
  where
    keysFrom (ColumnRef name) (DataFrame cols) =
        case lookup name cols of
            Just c -> Just (columnAsText c)
            Nothing -> Nothing

gridKeys ::
    [ColumnRef] -> [ColumnRef] -> DataFrame -> [Layer] -> ([Text], [Text])
gridKeys rowRefs colRefs chartFrame _layers =
    let rowKeys = compoundKeys rowRefs chartFrame
        colKeys = compoundKeys colRefs chartFrame
     in (rowKeys, colKeys)
  where
    compoundKeys refs frame
        | null refs = []
        | otherwise =
            let perCol = [maybe [] columnAsText (lookupColumnByRef r frame) | r <- refs]
                rows =
                    case perCol of
                        [] -> []
                        (xs : _) -> map (compoundAt perCol) [0 .. length xs - 1]
             in List.nub rows
    compoundAt cols i =
        Text.intercalate "\x00" [if i < length c then c !! i else "" | c <- cols]
    lookupColumnByRef (ColumnRef n) (DataFrame cols) = lookup n cols

filterFrameByKey :: ColumnRef -> Text -> DataFrame -> DataFrame
filterFrameByKey (ColumnRef name) key df@(DataFrame cols) =
    case lookup name cols of
        Nothing -> df
        Just c ->
            let texts = columnAsText c
                ixs = [i | (i, v) <- zip [0 ..] texts, v == key]
             in filterByRows ixs df

filterChartByKey :: Chart -> ColumnRef -> Text -> (DataFrame, [Layer])
filterChartByKey chart colRef key =
    let frame' = filterFrameByKey colRef key (chartData chart)
        filterLayer l =
            l{layerData = fmap (filterFrameByKey colRef key) (layerData l)}
        layers' = map filterLayer (chartLayers chart)
     in (frame', layers')

runLayer ::
    Theme ->
    PlotBox ->
    Projector ->
    [ColorSpec] ->
    Maybe [(Text, ColorSpec)] ->
    Int ->
    DataFrame ->
    Layer ->
    [Mark]
runLayer theme box proj palette colorMap ix globalFrame layer =
    let frame0 = fromMaybe globalFrame (layerData layer)
        framePostStat = applyStat (layerStat layer) (layerMapping layer) frame0
        frame = applyPosition (layerPosition layer) (layerMapping layer) framePostStat
        m = layerMapping layer
        defaults = layerAesDef layer
        colorSpec = case defColor defaults of
            Just c -> c
            Nothing -> layerDefaultColor palette ix layer
        col = specToColor colorSpec
        radius = case defSize defaults of
            Just r -> r
            Nothing -> pointSize theme layer
        alpha = fromMaybe 1 (defAlpha defaults)
        lineW = fromMaybe 2 (defLineWidth defaults)
        -- Per-row point colours: a mapped categorical 'aesColor' (when
        -- 'colorMap' is set for this layer) overrides the constant colour.
        pointColors = case colorMap of
            Just levelColors
                | Just cats <- categoricalColorColumn frame m ->
                    map (\cat -> specToColor (fromMaybe colorSpec (lookup cat levelColors))) cats
            _ -> repeat col
     in case layerGeom layer of
            GeomPoint -> drawPoints proj frame m pointColors radius alpha
            GeomLine -> drawLine proj frame m colorSpec lineW
            GeomBar -> drawBars proj frame m col
            GeomCol -> drawBars proj frame m col
            GeomHistogram -> drawBars proj frame m col
            GeomRibbon -> drawRibbon proj frame m col
            GeomErrorbar -> drawErrorbar proj frame m col
            GeomTile -> drawTiles proj frame m col
            GeomBoxplot -> drawBoxplot proj frame m col
            GeomDensity -> drawDensity proj frame m col
            GeomText -> drawText proj frame m col (themeFontSize theme)
            GeomArc -> drawArcs box frame m palette

drawPoints ::
    Projector ->
    DataFrame ->
    Mapping ->
    [Color] ->
    Double ->
    Double ->
    [Mark]
drawPoints proj frame m cols r alpha =
    case (resolveNumColumn frame (aesX m), resolveNumColumn frame (aesY m)) of
        (Just xv, Just yv) ->
            [ MCircle
                (proj x y)
                r
                defaultStyle
                    { styleFill = Just c
                    , styleFillOpacity = alpha
                    }
            | ((x, y), c) <- zip (zip xv yv) cols
            ]
        _ -> []

drawLine ::
    Projector ->
    DataFrame ->
    Mapping ->
    ColorSpec ->
    Double ->
    [Mark]
drawLine proj frame m col lineW =
    case (resolveNumColumn frame (aesX m), resolveNumColumn frame (aesY m)) of
        (Just xv, Just yv) ->
            let sorted = List.sortOn fst (zip xv yv)
                pts = [proj x y | (x, y) <- sorted]
             in [ MPolyline
                    pts
                    defaultStyle
                        { styleStroke = Just (specToColor col)
                        , styleStrokeWidth = lineW
                        }
                ]
        _ -> []

drawBars :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawBars proj frame m col =
    case (resolveNumColumn frame (aesX m), resolveNumColumn frame (aesY m)) of
        (Just xs, Just ys) ->
            let bases = case lookupColumn "__ybase" frame >>= columnAsNum of
                    Just bs | length bs == length xs -> bs
                    _ -> replicate (length xs) 0
                w = barWidth xs
                halfW = w / 2
             in [ rectFromCorners
                    (proj (x - halfW) y0)
                    (proj (x + halfW) y1)
                    col
                | (x, y1, y0) <- zip3 xs ys bases
                ]
        _ -> []

barWidth :: [Double] -> Double
barWidth xs
    | length unique < 2 = 0.8
    | otherwise =
        let diffs = zipWith (-) (drop 1 unique) unique
            pos = filter (> 1e-9) diffs
         in if null pos then 0.8 else minimum pos * 0.8
  where
    unique = List.nub (List.sort xs)

rectFromCorners :: Point -> Point -> Color -> Mark
rectFromCorners (Point xa ya) (Point xb yb) col =
    let x0 = min xa xb
        y0 = min ya yb
        w = abs (xa - xb)
        h = abs (ya - yb)
     in MRect (Rect x0 y0 w h) defaultStyle{styleFill = Just col}

drawRibbon :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawRibbon proj frame m col =
    case ( resolveNumColumn frame (aesX m)
         , resolveNumColumn frame (aesYmin m)
         , resolveNumColumn frame (aesYmax m)
         ) of
        (Just xs, Just lo, Just hi) ->
            let triples = List.sortOn (\(a, _, _) -> a) (zip3 xs lo hi)
                topPts = [proj x h | (x, _, h) <- triples]
                botPts = reverse [proj x l | (x, l, _) <- triples]
             in [ MPolygon
                    (topPts <> botPts)
                    defaultStyle
                        { styleFill = Just col
                        , styleFillOpacity = 0.4
                        , styleStroke = Just col
                        , styleStrokeWidth = 1
                        }
                ]
        _ -> []

drawErrorbar :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawErrorbar proj frame m col =
    case ( resolveNumColumn frame (aesX m)
         , resolveNumColumn frame (aesYmin m)
         , resolveNumColumn frame (aesYmax m)
         ) of
        (Just xs, Just lo, Just hi) ->
            let capW = barWidth xs * 0.4
                halfW = capW / 2
                style = defaultStyle{styleStroke = Just col, styleStrokeWidth = 1.5}
             in concat
                    [ [ MPolyline [proj x l, proj x h] style
                      , MPolyline [proj (x - halfW) l, proj (x + halfW) l] style
                      , MPolyline [proj (x - halfW) h, proj (x + halfW) h] style
                      ]
                    | (x, l, h) <- zip3 xs lo hi
                    ]
        _ -> []

drawTiles :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawTiles proj frame m col =
    case (resolveNumColumn frame (aesX m), resolveNumColumn frame (aesY m)) of
        (Just xs, Just ys) ->
            let wX = barWidth xs / 0.8
                wY = barWidth ys / 0.8
                halfX = wX / 2
                halfY = wY / 2
                fillCol = resolveNumColumn frame (aesFill m)
                fillRange = case fillCol of
                    Just vs | not (null vs) -> (minimum vs, maximum vs)
                    _ -> (0, 1)
                colorFor i = case fillCol of
                    Just vs
                        | i < length vs ->
                            let (lo, hi) = fillRange
                                t =
                                    if hi == lo
                                        then 0.5
                                        else (vs !! i - lo) / (hi - lo)
                             in gradientColor t
                    _ -> col
             in [ rectFromCorners
                    (proj (x - halfX) (y - halfY))
                    (proj (x + halfX) (y + halfY))
                    (colorFor i)
                | (i, (x, y)) <- zip [0 :: Int ..] (zip xs ys)
                ]
        _ -> []

gradientColor :: Double -> Color
gradientColor t =
    let palette =
            [ Blue
            , BrightBlue
            , BrightCyan
            , BrightGreen
            , BrightYellow
            , BrightRed
            ]
        n = length palette
        clamped = max 0 (min 0.9999 t)
        ix = floor (clamped * fromIntegral n) :: Int
     in palette !! ix

drawBoxplot :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawBoxplot proj frame m col =
    case ( resolveNumColumn frame (aesX m)
         , lookupColumn "__ymin" frame >>= columnAsNum
         , lookupColumn "__q1" frame >>= columnAsNum
         , lookupColumn "__median" frame >>= columnAsNum
         , lookupColumn "__q3" frame >>= columnAsNum
         , lookupColumn "__ymax" frame >>= columnAsNum
         ) of
        (Just xs, Just mins, Just qq1, Just meds, Just qq3, Just maxs) ->
            let w = barWidth xs
                halfW = w / 2
                style = defaultStyle{styleStroke = Just col, styleStrokeWidth = 1}
                fillStyle = style{styleFill = Just col, styleFillOpacity = 0.2}
             in concat
                    [ [ MPolygon
                            [ proj (x - halfW) qLo
                            , proj (x + halfW) qLo
                            , proj (x + halfW) qHi
                            , proj (x - halfW) qHi
                            ]
                            fillStyle
                      , MPolyline
                            [proj (x - halfW) medV, proj (x + halfW) medV]
                            style{styleStrokeWidth = 2}
                      , MPolyline [proj x qLo, proj x ymin] style
                      , MPolyline [proj x qHi, proj x ymax] style
                      ]
                    | (x, ymin, qLo, medV, qHi, ymax) <- List.zip6 xs mins qq1 meds qq3 maxs
                    ]
        _ -> []

drawDensity :: Projector -> DataFrame -> Mapping -> Color -> [Mark]
drawDensity proj frame m col =
    drawLine proj frame m (NamedColor col) 2

drawText :: Projector -> DataFrame -> Mapping -> Color -> Double -> [Mark]
drawText proj frame m col size =
    case ( resolveNumColumn frame (aesX m)
         , resolveNumColumn frame (aesY m)
         , aesLabel m
         ) of
        (Just xs, Just ys, Just (ColumnRef labelCol)) ->
            case lookupColumn labelCol frame of
                Just labelColData ->
                    let labels = columnAsText labelColData
                        triples = zip3 xs ys labels
                     in [ MText
                            (proj x y)
                            lbl
                            defaultTextStyle
                                { textFill = col
                                , textSize = size
                                , textAnchor = AnchorMiddle
                                }
                        | (x, y, lbl) <- triples
                        ]
                Nothing -> []
        _ -> []

drawArcs :: PlotBox -> DataFrame -> Mapping -> [ColorSpec] -> [Mark]
drawArcs box frame m palette =
    case resolveNumColumn frame (aesY m) of
        Just vs
            | not (null vs) ->
                let total = sum vs
                    cx = boxX box + boxW box / 2
                    cy = boxY box + boxH box / 2
                    r = min (boxW box) (boxH box) / 2 * 0.9
                    fracs = if total == 0 then map (const 0) vs else map (/ total) vs
                    starts = scanl (+) (-(pi / 2)) (map (* (2 * pi)) fracs)
                    ends = drop 1 starts
                    colorAt i = palette !! (i `mod` max 1 (length palette))
                 in [ MArc
                        (Point cx cy)
                        r
                        s
                        e
                        defaultStyle
                            { styleFill = Just (specToColor (colorAt i))
                            , styleStroke = Just (specToColor (NamedColor BrightBlack))
                            , styleStrokeWidth = 1
                            }
                    | (i, (s, e)) <- zip [0 :: Int ..] (zip starts ends)
                    ]
        _ -> []

{- | Resolve a column to numeric values for projection. Categorical
columns map each row to its position in the unique-value list, so
repeated values share an X position (needed for stacked / grouped
/ faceted bars).
-}
resolveNumColumn :: DataFrame -> Maybe ColumnRef -> Maybe [Double]
resolveNumColumn _ Nothing = Nothing
resolveNumColumn df (Just (ColumnRef n)) =
    case lookupColumn n df of
        Just (ColCat xs) -> Just (categoricalIndices xs)
        Just c -> columnAsNum c
        Nothing -> Nothing

categoricalIndices :: [Text] -> [Double]
categoricalIndices xs =
    let uniques = List.nub xs
        indexOf x = maybe 0 fromIntegral (List.elemIndex x uniques)
     in map indexOf xs

{- | Range across one or more aesthetic getters; the Y-axis caller
folds @aesY@ + @aesYmin@ + @aesYmax@ + the conventional internal
columns so ribbon / errorbar / boxplot layers (which leave 'aesY'
unset) still get a sensible Y range. Stat and position are applied
so histogram-style @count@ columns appear.
-}
unionDataRange ::
    [Mapping -> Maybe ColumnRef] ->
    [Layer] ->
    DataFrame ->
    (Double, Double)
unionDataRange getRefs layers globalFrame =
    let pulls =
            [ values
            | layer <- layers
            , let m = layerMapping layer
                  base = fromMaybe globalFrame (layerData layer)
                  stat' = applyStat (layerStat layer) m base
                  frame = applyPosition (layerPosition layer) m stat'
            , getRef <- getRefs
            , Just values <- [resolveNumColumn frame (getRef m)]
            ]
        all_ = concat pulls
     in if null all_
            then (0, 1)
            else (minimum all_, maximum all_)

{- | Range with geom adjustments: bar/col/histogram/tile pad ±halfWidth
around (x, y) so boundary rects don't overflow the plot box; bars
also extend the Y range to include their @__ybase@ (default 0) so a
horizontal bar chart with all-positive values doesn't put its
baseline outside the scale.
-}
paddedRange ::
    [Layer] ->
    DataFrame ->
    [Mapping -> Maybe ColumnRef] ->
    Bool ->
    (Double, Double)
paddedRange layers frame getRefs isXAxis =
    let (lo, hi) = unionDataRange getRefs layers frame
        primary = case getRefs of
            (g : _) -> g
            [] -> const Nothing
        pad = geomAxisPadding primary isXAxis layers frame
        loPadded = lo - pad
        hiPadded = hi + pad
     in case barAxisAnchor isXAxis layers frame of
            Nothing -> (loPadded, hiPadded)
            Just anchor -> (min loPadded anchor, max hiPadded anchor)

yRangeRefs :: [Mapping -> Maybe ColumnRef]
yRangeRefs =
    [ aesY
    , aesYmin
    , aesYmax
    , const (Just (ColumnRef "__ymin"))
    , const (Just (ColumnRef "__q1"))
    , const (Just (ColumnRef "__median"))
    , const (Just (ColumnRef "__q3"))
    , const (Just (ColumnRef "__ymax"))
    , const (Just (ColumnRef "__ybase"))
    ]

xRangeRefs :: [Mapping -> Maybe ColumnRef]
xRangeRefs = [aesX]

{- | When an aesthetic resolves to a 'ColCat' column, override the
trained scale's breaks and labels so each category sits at its
integer index with the category text as its tick label. Numeric
data passes through unchanged.
-}
applyCategorical ::
    (Mapping -> Maybe ColumnRef) ->
    [Layer] ->
    DataFrame ->
    TrainedScale ->
    TrainedScale
applyCategorical getRef layers globalFrame ts =
    case categoricalLabelsFor getRef layers globalFrame of
        Nothing -> ts
        Just labels ->
            let breaks = [fromIntegral i | i <- [0 .. length labels - 1 :: Int]]
             in ts{tsBreaks = breaks, tsLabels = labels}

{- | Read /pre-stat/ — a stat like 'Granite.Spec.StatBoxplot' consumes the
categorical X and emits numeric indices, but the labels we want
are the user's original group names.
-}
categoricalLabelsFor ::
    (Mapping -> Maybe ColumnRef) ->
    [Layer] ->
    DataFrame ->
    Maybe [Text]
categoricalLabelsFor getRef layers globalFrame = findFirst layers
  where
    findFirst [] = Nothing
    findFirst (layer : rest) =
        let m = layerMapping layer
            frame = fromMaybe globalFrame (layerData layer)
         in case getRef m of
                Just (ColumnRef n) -> case lookupColumn n frame of
                    Just (ColCat xs) -> Just (List.nub xs)
                    _ -> findFirst rest
                Nothing -> findFirst rest

geomAxisPadding ::
    (Mapping -> Maybe ColumnRef) ->
    Bool ->
    [Layer] ->
    DataFrame ->
    Double
geomAxisPadding getRef isXAxis layers globalFrame =
    let perLayerPad layer
            | not (needsPadding (layerGeom layer) isXAxis) = 0
            | otherwise =
                let m = layerMapping layer
                    base = fromMaybe globalFrame (layerData layer)
                    postStat = applyStat (layerStat layer) m base
                    postPos = applyPosition (layerPosition layer) m postStat
                 in case resolveNumColumn postPos (getRef m) of
                        Just vs -> barWidth vs / 2
                        Nothing -> 0
        pads = map perLayerPad layers
     in if null pads then 0 else maximum (0 : pads)

needsPadding :: Geom -> Bool -> Bool
needsPadding GeomBar isX = isX
needsPadding GeomCol isX = isX
needsPadding GeomHistogram isX = isX
needsPadding GeomBoxplot isX = isX
needsPadding GeomErrorbar isX = isX
needsPadding GeomTile _ = True
needsPadding _ _ = False

{- | Bar baseline: the minimum of @__ybase@ across bar-like layers
(default 0), so an all-positive chart still includes 0 at the
bottom of the bars.
-}
barAxisAnchor :: Bool -> [Layer] -> DataFrame -> Maybe Double
barAxisAnchor isXAxis layers globalFrame
    | isXAxis = Nothing
    | otherwise =
        let bases = [anchorFor layer | layer <- layers, isBarLike (layerGeom layer)]
         in case bases of
                [] -> Nothing
                xs -> Just (minimum (0 : xs))
  where
    isBarLike GeomBar = True
    isBarLike GeomCol = True
    isBarLike GeomHistogram = True
    isBarLike _ = False

    anchorFor layer =
        let m = layerMapping layer
            base = fromMaybe globalFrame (layerData layer)
            postStat = applyStat (layerStat layer) m base
            postPos = applyPosition (layerPosition layer) m postStat
         in case lookupColumn "__ybase" postPos >>= columnAsNum of
                Just bs | not (null bs) -> minimum bs
                _ -> 0

layerDefaultColor :: [ColorSpec] -> Int -> Layer -> ColorSpec
layerDefaultColor palette ix _layer
    | null palette = NamedColor BrightBlue
    | otherwise = palette !! (ix `mod` length palette)

pointSize :: Theme -> Layer -> Double
pointSize _ _ = 3

{- | Map the distinct values of a categorical column to palette colours, in
first-seen order; shared by the renderer and 'collectLegend' so points and the
legend agree.
-}
categoricalColorMap :: [ColorSpec] -> [Text] -> [(Text, ColorSpec)]
categoricalColorMap palette levels =
    [(lvl, colorAt i) | (i, lvl) <- zip [0 :: Int ..] levels]
  where
    colorAt i
        | null palette = NamedColor BrightBlue
        | otherwise = palette !! (i `mod` length palette)

{- | The per-category colour map for a layer, when its 'aesColor' maps to a
categorical ('ColCat') column. Computed once from the whole-chart layer frame so
colours stay consistent across facets and the legend; 'Nothing' otherwise
(numeric colour columns keep the single per-layer colour).
-}
layerColorMap :: [ColorSpec] -> DataFrame -> Layer -> Maybe [(Text, ColorSpec)]
layerColorMap palette globalFrame layer
    | GeomPoint <- layerGeom layer
    , Just cats <-
        categoricalColorColumn
            (fromMaybe globalFrame (layerData layer))
            (layerMapping layer) =
        Just (categoricalColorMap palette (List.nub cats))
    | otherwise = Nothing

{- | The categorical ('ColCat') colour column's per-row values, if 'aesColor'
maps to one.
-}
categoricalColorColumn :: DataFrame -> Mapping -> Maybe [Text]
categoricalColorColumn frame m = case aesColor m of
    Just (ColumnRef n) -> case lookupColumn n frame of
        Just (ColCat xs) -> Just xs
        _ -> Nothing
    Nothing -> Nothing

{- | Legend entries. A categorical-'aesColor' layer contributes one entry per
category value (colours matching the points via 'layerColorMap'); 'GeomText'
layers contribute none; any other layer keeps a single per-layer swatch.
-}
collectLegend ::
    [Layer] -> [ColorSpec] -> [Maybe [(Text, ColorSpec)]] -> [(Text, ColorSpec)]
collectLegend layers palette colorMaps =
    concat
        [ entriesFor ix layer cmap
        | (ix, layer, cmap) <- zip3 [0 :: Int ..] layers (colorMaps ++ repeat Nothing)
        ]
  where
    entriesFor ix layer cmap
        | GeomText <- layerGeom layer = []
        | Just levelColors <- cmap = levelColors
        | otherwise =
            [(legendName ix layer, palette !! (ix `mod` max 1 (length palette)))]
    legendName ix layer =
        case aesGroup (layerMapping layer) of
            Just (ColumnRef n) -> n
            Nothing -> "series " <> Text.pack (show ix)

{- | Resolve a 'ColorSpec' to an exact RGB 'Color'. The SVG backend renders
this exactly (via @colorHex@); the terminal backend quantises it to the nearest
ANSI slot (via @ansiCode@).
-}
specToColor :: ColorSpec -> Color
specToColor (NamedColor c) = c
specToColor (RGB r g b) = Color r g b
specToColor (Hex t) = fromMaybe Black (parseHex t)
