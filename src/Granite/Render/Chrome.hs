{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Render.Chrome
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Backend-agnostic chart chrome: axes, title, legend. Each helper
returns a list of primitive 'Mark's.
-}
module Granite.Render.Chrome (
    PlotBox (..),
    Margins (..),
    computePlotBox,
    AxisLayout (..),
    XLabelMode,
    xModeNoRotate,
    axisLayout,
    domainLabels,
    axesMarks,
    titleMarks,
    legendMarks,
) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Granite.Internal.Util (estLabelWidthPx, truncatePx)
import Text.Read (readMaybe)

import Granite.Color (Color (..))
import Granite.Render.Scene (
    Mark (..),
    Point (..),
    Rect (..),
    Style (..),
    TextAnchor (..),
    TextStyle (..),
    defaultStyle,
    defaultTextStyle,
 )
import Granite.Scale (TrainedScale (..))
import Granite.Spec (
    ColorSpec (..),
    Coord (..),
    PolarAes (..),
    PolarDir (..),
    Size (..),
    Theme (..),
 )

{- | Pixel rectangle the chart body occupies, plus the overall scene
bounds. Margins around the plot area host title, axis labels, legend.
-}
data PlotBox = PlotBox
    { boxSceneW :: !Double
    , boxSceneH :: !Double
    , boxX :: !Double
    , boxY :: !Double
    , boxW :: !Double
    , boxH :: !Double
    }
    deriving (Eq, Show)

-- | Scene pixel dimensions for a 'Size'.
sceneDims :: Size -> Theme -> (Double, Double)
sceneDims sz theme = case sz of
    SizeChars w h ->
        (fromIntegral w * themePxPerChar theme, fromIntegral h * themePxPerLine theme)
    SizePixels w h -> (fromIntegral w, fromIntegral h)
    SizeResponsive ar -> let h = 320 in (h * ar, h)

{- | Inputs that grow the plot margins beyond the theme defaults. @marExtraBottom@
enlarges the bottom margin (for rotated x-labels) and @marLeftWanted@ requests a
left margin (to fit long y-labels / the rotated labels' leftward reach); both
fall back to the defaults when 0\/small.
-}
data Margins = Margins
    { marHasTitle :: !Bool
    , marHasRightLegend :: !Bool
    , marHasBottomLegend :: !Bool
    , marExtraBottom :: !Double
    , marLeftWanted :: !Double
    }
    deriving (Eq, Show)

computePlotBox :: Size -> Theme -> Margins -> PlotBox
computePlotBox sz theme m =
    let (sceneW, sceneH) = sceneDims sz theme
        topMargin = if marHasTitle m then themeTitleSize theme + 20 else 10
        bottomMargin =
            (if marHasBottomLegend m then 30 else 0)
                + max (themeFontSize theme * 1.8 + 8) (marExtraBottom m)
        leftMargin = max (themeFontSize theme * 5) (marLeftWanted m)
        rightMargin = rightMarginPx (marHasRightLegend m)
        plotW = max 1 (sceneW - leftMargin - rightMargin)
        plotH = max 1 (sceneH - topMargin - bottomMargin)
     in PlotBox sceneW sceneH leftMargin topMargin plotW plotH

rightMarginPx :: Bool -> Double
rightMarginPx hasRightLegend = if hasRightLegend then 120 else 20

-- | The automatic policy for bottom-axis labels given their per-tick budget.
data XPolicy = XUpright | XThin | XRotate
    deriving (Eq, Show)

xLabelPolicy :: Double -> Double -> [Text] -> XPolicy
xLabelPolicy fontSize budget labels
    | maxLabelWidth fontSize labels <= budget = XUpright
    | not (null labels) && all isNumericLabel labels = XThin
    | otherwise = XRotate

maxLabelWidth :: Double -> [Text] -> Double
maxLabelWidth fontSize = maximum . (0 :) . map (estLabelWidthPx fontSize)

-- | Rotated x-labels wider than this (px) are truncated with a hover @\<title\>@.
rotatedLabelLimitPx :: Double
rotatedLabelLimitPx = 130

-- | @sin 45°@ — a rotated label's horizontal/vertical reach as a fraction of its width.
sin45 :: Double
sin45 = sqrt 2 / 2

{- | How the bottom (x) axis labels are laid out, plus the extra margins that
choice needs. Decided exactly once per chart so margin sizing and the actual
label rendering can never disagree (the bug when each recomputed the policy
from a slightly different plot width). 'XLabelMode' is threaded to 'axesMarks';
'alExtraBottom'\/'alLeftWanted' feed 'computePlotBox'.
-}
data AxisLayout = AxisLayout
    { alMode :: !XLabelMode
    , alExtraBottom :: !Double
    , alLeftWanted :: !Double
    }

{- | What a panel's x-axis renderer should do. 'XFixed' carries the
chart-global decision verbatim (single panel). 'XLocalNoRotate' lets a faceted
panel pick upright\/thin from its own width, but never rotate — rotation needs a
per-cell margin reservation the facet layout doesn't make.
-}
data XLabelMode = XFixed !XPolicy | XLocalNoRotate

-- | The mode for axes that never rotate their labels (polar; also the faceted default).
xModeNoRotate :: XLabelMode
xModeNoRotate = XLocalNoRotate

{- | Decide the bottom-axis label policy and the margins it needs, in one pass.
A wide left label grows the left margin (up to ~42% of the scene); rotated
bottom labels also reach down (bottom margin) and left (left margin) by
@sin 45°@ of their width. Faceted charts never rotate, so they reserve no extra
bottom margin.
-}
axisLayout :: Theme -> Size -> Bool -> Bool -> [Text] -> [Text] -> AxisLayout
axisLayout theme sz hasRightLegend faceted bottomLbls leftLbls =
    AxisLayout
        { alMode = mode
        , alExtraBottom = bottomExtra
        , alLeftWanted = leftWanted
        }
  where
    (sceneW, _) = sceneDims sz theme
    fontSize = themeFontSize theme
    cap = 0.42 * sceneW
    -- Left margin from y-labels alone, used to set the plot width that decides
    -- x rotation. Rotation can grow it further (its leftward reach), but the
    -- rotate/thin decision itself is fixed here and threaded, so it never drifts.
    provLeft = min cap (max (fontSize * 5) (maxLabelWidth fontSize leftLbls + 12))
    provPlotW = max 1 (sceneW - provLeft - rightMarginPx hasRightLegend)
    n = length bottomLbls
    budget = if n <= 1 then provPlotW else provPlotW / fromIntegral n
    policy = xLabelPolicy fontSize budget bottomLbls
    rotates = not faceted && policy == XRotate
    rotatedExtent = sin45 * min (maxLabelWidth fontSize bottomLbls) rotatedLabelLimitPx
    mode = if faceted then XLocalNoRotate else XFixed policy
    -- A rotated label's baseline sits @fontSize + 4@ below the axis and reaches
    -- down a further @rotatedExtent@; +8 keeps a descender's clearance above the
    -- scene edge.
    bottomExtra = if rotates then fontSize + 4 + rotatedExtent + 8 else 0
    leftWanted = min cap (max provLeft (if rotates then rotatedExtent else 0))

axesMarks ::
    Theme ->
    PlotBox ->
    Coord ->
    XLabelMode ->
    TrainedScale ->
    TrainedScale ->
    [Mark]
axesMarks theme box coord xmode xs ys = case coord of
    CoordCartesian -> cartesianAxes theme box False xmode xs ys
    -- CoordFlip rotates 90° CW so the Y axis grows top-down.
    CoordFlip -> cartesianAxes theme box True xmode ys xs
    CoordPolar aes a0 dir -> polarAxes theme box aes a0 dir xs ys

{- | Axes are 'MAxisLine' spines (clean box-drawing chars in terminal,
@<line>@ in SVG). Gridlines and tick stubs are omitted so the
terminal output stays uncluttered; tick /labels/ sit beside each
break. @flipY@ inverts Y for 'CoordFlip'.
-}
cartesianAxes ::
    Theme -> PlotBox -> Bool -> XLabelMode -> TrainedScale -> TrainedScale -> [Mark]
cartesianAxes theme box flipY xmode xs ys =
    let axisColor = colorOfSpec (themeAxisColor theme)
        textColor = colorOfSpec (themeTextColor theme)

        px = boxX box
        py = boxY box
        pw = boxW box
        ph = boxH box

        spineStyle = defaultStyle{styleStroke = Just axisColor, styleStrokeWidth = 1}

        xAxis = MAxisLine (Point px (py + ph)) (Point (px + pw) (py + ph)) spineStyle
        yAxis = MAxisLine (Point px py) (Point px (py + ph)) spineStyle

        xLabels = xAxisLabels theme px py pw ph textColor xmode xs
        yLabels = yAxisLabels theme px py ph flipY textColor ys
     in [xAxis, yAxis] <> xLabels <> yLabels

{- | X-axis tick labels that automatically adapt to long words, following the
chart-global 'XLabelMode' so margin sizing and rendering agree. If the labels
fit their per-tick slot they render upright (unchanged). Otherwise, keyed on the
data: numeric labels are thinned to a subset that fits (dropping ticks on a
continuum is fine, and the first\/last are always kept so the axis bounds stay
labelled), while categorical labels are rotated −45° and, past
'rotatedLabelLimitPx', truncated with a hover @\<title\>@ — a category name is
never silently dropped. Rotation and the @\<title\>@ tooltip are SVG-only; the
terminal backend draws the (truncated) text upright.
-}
xAxisLabels ::
    Theme ->
    Double ->
    Double ->
    Double ->
    Double ->
    Color ->
    XLabelMode ->
    TrainedScale ->
    [Mark]
xAxisLabels theme px py pw ph textColor xmode xs = case resolvedPolicy of
    XUpright -> map (upright budget) raw
    XThin ->
        let keep = thinIndices n maxW pw
            slot = pw / fromIntegral (max 1 (length keep))
         in [upright slot (raw !! i) | i <- keep]
    XRotate -> map rotated raw
  where
    fontSize = themeFontSize theme
    baseY = py + ph + fontSize + 4
    raw =
        [ (px + tsProject xs v * pw, lbl)
        | (v, lbl) <- zip (tsBreaks xs) (tsLabels xs)
        , inDomain (tsDomain xs) v
        ]
    n = length raw
    maxW = maxLabelWidth fontSize (map snd raw)
    budget = if n <= 1 then pw else pw / fromIntegral n
    resolvedPolicy = case xmode of
        XFixed p -> p
        -- Faceted: pick from this panel's own width but never rotate.
        XLocalNoRotate -> case xLabelPolicy fontSize budget (map snd raw) of
            XRotate -> XUpright
            p -> p
    sty anchor rot title =
        defaultTextStyle
            { textFill = textColor
            , textSize = fontSize
            , textAnchor = anchor
            , textRotate = rot
            , textTitle = title
            }
    -- Upright labels are truncated to their per-tick slot so they never overrun
    -- it — the only path that bites is the faceted fallback, where rotation is
    -- forbidden and a long category would otherwise overflow its cell.
    upright slot (xp, full) =
        let (lbl, title) = truncatePx fontSize slot full
         in MText (Point xp baseY) lbl (sty AnchorMiddle 0 title)
    rotated (xp, full) =
        let (lbl, title) = truncatePx fontSize rotatedLabelLimitPx full
         in MText (Point xp baseY) lbl (sty AnchorEnd (-45) title)

{- | Indices of a thinned label subset: the most evenly-spaced labels that fit
@plotW@ at width @maxW@, always including the first and last so the axis bounds
stay labelled. (Even-index thinning could drop the max tick on an even count.)
-}
thinIndices :: Int -> Double -> Double -> [Int]
thinIndices n maxW plotW
    | n <= 2 = [0 .. n - 1]
    | otherwise =
        let fit = max 2 (min n (floor (plotW / max 1 maxW)))
            pick j = round (fromIntegral (j * (n - 1)) / fromIntegral (fit - 1) :: Double)
         in nub (map pick [0 .. fit - 1])

{- | Y-axis (left) tick labels, one per break. Truncated with a hover
@\<title\>@ to fit the left margin (the margin itself is grown to hold long
labels up to a cap; this only bites past that cap).
-}
yAxisLabels ::
    Theme -> Double -> Double -> Double -> Bool -> Color -> TrainedScale -> [Mark]
yAxisLabels theme px py ph flipY textColor ys =
    [ MText (Point (px - 6) (yPos + fontSize / 3)) lbl (sty title)
    | (v, full) <- zip (tsBreaks ys) (tsLabels ys)
    , inDomain (tsDomain ys) v
    , let yPos =
            if flipY then py + tsProject ys v * ph else py + ph - tsProject ys v * ph
          (lbl, title) = truncatePx fontSize (px - 10) full
    ]
  where
    fontSize = themeFontSize theme
    sty title =
        defaultTextStyle
            { textFill = textColor
            , textSize = fontSize
            , textAnchor = AnchorEnd
            , textTitle = title
            }

{- | A label produced by a numeric axis — only decimal\/scientific digits,
so categorical values like @\"NaN\"@, @\"Infinity\"@ or @\"0x10\"@ (which
'readMaybe' would otherwise accept) are correctly treated as categories.
-}
isNumericLabel :: Text -> Bool
isNumericLabel t =
    not (Text.null s)
        && Text.all (`elem` ("0123456789.eE+-" :: String)) s
        && isJust (readMaybe (Text.unpack s) :: Maybe Double)
  where
    s = Text.strip t

{- | Concentric rings (radial breaks) + radial spokes (angular breaks)
+ tick labels. Centred in the plot box with radius = half its
shortest side.
-}
polarAxes ::
    Theme ->
    PlotBox ->
    PolarAes ->
    Double ->
    PolarDir ->
    TrainedScale ->
    TrainedScale ->
    [Mark]
polarAxes theme box aes a0 dir xs ys =
    let cx = boxX box + boxW box / 2
        cy = boxY box + boxH box / 2
        maxR = min (boxW box) (boxH box) / 2
        gridColor = colorOfSpec (themeGridColor theme)
        axisColor = colorOfSpec (themeAxisColor theme)
        textColor = colorOfSpec (themeTextColor theme)
        sign = case dir of
            PolarCW -> 1
            PolarCCW -> -1 :: Double

        (thetaScale, radialScale) = case aes of
            ThetaX -> (xs, ys)
            ThetaY -> (ys, xs)

        circleAt :: Double -> Maybe Color -> Double -> Mark
        circleAt rPx stroke widthPx =
            let nSeg = 64 :: Int
                pts =
                    [ Point (cx + rPx * cos a) (cy + rPx * sin a)
                    | i <- [0 .. nSeg]
                    , let a = (fromIntegral i / fromIntegral nSeg) * 2 * pi
                    ]
             in MPolyline pts defaultStyle{styleStroke = stroke, styleStrokeWidth = widthPx}

        rings =
            [ circleAt (tsProject radialScale v * maxR) (Just gridColor) 0.5
            | v <- tsBreaks radialScale
            , inDomain (tsDomain radialScale) v
            , tsProject radialScale v > 1e-6
            ]

        outerRing = circleAt maxR (Just axisColor) 1

        spokeFor v =
            let t = tsProject thetaScale v
                theta = a0 + sign * t * 2 * pi
                ox = cx + maxR * cos theta
                oy = cy + maxR * sin theta
             in MPolyline
                    [Point cx cy, Point ox oy]
                    defaultStyle{styleStroke = Just gridColor, styleStrokeWidth = 0.5}
        spokes =
            [ spokeFor v
            | v <- tsBreaks thetaScale
            , inDomain (tsDomain thetaScale) v
            ]

        thetaLabelOffset = 10
        thetaLabels =
            [ MText
                ( Point
                    (cx + (maxR + thetaLabelOffset) * cos theta)
                    (cy + (maxR + thetaLabelOffset) * sin theta + themeFontSize theme / 3)
                )
                lbl
                defaultTextStyle
                    { textFill = textColor
                    , textSize = themeFontSize theme
                    , textAnchor = AnchorMiddle
                    }
            | (v, lbl) <- zip (tsBreaks thetaScale) (tsLabels thetaScale)
            , inDomain (tsDomain thetaScale) v
            , let theta = a0 + sign * tsProject thetaScale v * 2 * pi
            ]

        rLabels =
            [ MText
                ( Point
                    (cx + rPx * cos a0 + 3)
                    (cy + rPx * sin a0 - 2)
                )
                lbl
                defaultTextStyle
                    { textFill = textColor
                    , textSize = themeFontSize theme
                    , textAnchor = AnchorStart
                    }
            | (v, lbl) <- zip (tsBreaks radialScale) (tsLabels radialScale)
            , inDomain (tsDomain radialScale) v
            , let rPx = tsProject radialScale v * maxR
            , rPx > 1e-6
            ]
     in rings <> spokes <> [outerRing] <> thetaLabels <> rLabels

inDomain :: (Double, Double) -> Double -> Bool
inDomain (lo, hi) v = v >= lo - 1e-9 && v <= hi + 1e-9

-- | A trained scale's in-domain tick labels, in break order.
domainLabels :: TrainedScale -> [Text]
domainLabels s = [lbl | (v, lbl) <- zip (tsBreaks s) (tsLabels s), inDomain (tsDomain s) v]

titleMarks :: Theme -> PlotBox -> Maybe Text -> [Mark]
titleMarks _ _ Nothing = []
titleMarks _ _ (Just t) | Text.null t = []
titleMarks theme box (Just t) =
    [ MText
        (Point (boxX box + boxW box / 2) (boxY box - 8))
        t
        defaultTextStyle
            { textFill = colorOfSpec (themeTextColor theme)
            , textSize = themeTitleSize theme
            , textAnchor = AnchorMiddle
            }
    ]

legendMarks :: Theme -> PlotBox -> [(Text, ColorSpec)] -> [Mark]
legendMarks theme box entries =
    let lx = boxX box + boxW box + 15
        ly = boxY box + 5
        rowH = themeFontSize theme + 6
     in concat
            [ let yy = ly + fromIntegral i * rowH
                  swatch =
                    MRect
                        (Rect lx yy 12 12)
                        defaultStyle{styleFill = Just (colorOfSpec col)}
                  label =
                    MText
                        (Point (lx + 16) (yy + themeFontSize theme - 1))
                        name
                        defaultTextStyle
                            { textFill = colorOfSpec (themeTextColor theme)
                            , textSize = themeFontSize theme
                            , textAnchor = AnchorStart
                            }
               in [swatch, label]
            | (i, (name, col)) <- zip [0 :: Int ..] entries
            ]

{- | Quantise a 'ColorSpec' to the nearest ANSI 'Color' for the
terminal backend; SVG reads RGB / Hex directly.
-}
colorOfSpec :: ColorSpec -> Color
colorOfSpec (NamedColor c) = c
colorOfSpec (RGB r g b) = nearestAnsi (fromIntegral r) (fromIntegral g) (fromIntegral b)
colorOfSpec (Hex h) =
    case parseHex h of
        Just (r, g, b) -> nearestAnsi r g b
        Nothing -> Default

parseHex :: Text -> Maybe (Double, Double, Double)
parseHex t =
    let s = Text.unpack (Text.dropWhile (== '#') t)
     in case s of
            [a, b, c, d, e, f] ->
                (,,) <$> fromHex2 [a, b] <*> fromHex2 [c, d] <*> fromHex2 [e, f]
            _ -> Nothing

fromHex2 :: String -> Maybe Double
fromHex2 [a, b] = do
    h <- digit a
    l <- digit b
    pure (fromIntegral (h * 16 + l))
  where
    digit ch
        | isDigit ch = Just (fromEnum ch - fromEnum '0')
        | ch >= 'a' && ch <= 'f' = Just (10 + fromEnum ch - fromEnum 'a')
        | ch >= 'A' && ch <= 'F' = Just (10 + fromEnum ch - fromEnum 'A')
        | otherwise = Nothing
fromHex2 _ = Nothing

{- | Nearest of the 16 ANSI colors using the canonical VGA palette
(not the pastel SVG hex values). Important for visibility: mid-gray
@#555555@ maps to 'BrightBlack' (a real grey), not 'Black' (which
would be invisible on a dark terminal).
-}
nearestAnsi :: Double -> Double -> Double -> Color
nearestAnsi r g b =
    let candidates =
            [ (Black, 0, 0, 0)
            , (Red, 170, 0, 0)
            , (Green, 0, 170, 0)
            , (Yellow, 170, 85, 0)
            , (Blue, 0, 0, 170)
            , (Magenta, 170, 0, 170)
            , (Cyan, 0, 170, 170)
            , (White, 170, 170, 170)
            , (BrightBlack, 85, 85, 85)
            , (BrightRed, 255, 85, 85)
            , (BrightGreen, 85, 255, 85)
            , (BrightYellow, 255, 255, 85)
            , (BrightBlue, 85, 85, 255)
            , (BrightMagenta, 255, 85, 255)
            , (BrightCyan, 85, 255, 255)
            , (BrightWhite, 255, 255, 255)
            ]
        dist (_, cr, cg, cb) = sq (r - cr) + sq (g - cg) + sq (b - cb)
        sq x = x * x
        pick (best, bd) cand =
            let d = dist cand
             in if d < bd then (fstC cand, d) else (best, bd)
        fstC (c, _, _, _) = c
     in fst (foldl pick (Default, 1 / 0) candidates)
