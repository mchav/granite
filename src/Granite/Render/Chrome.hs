
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
    computePlotBox,
    axesMarks,
    titleMarks,
    legendMarks,
) where

import Data.Text (Text)
import Data.Text qualified as Text

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
    pxPerChar,
    pxPerLine,
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

computePlotBox :: Size -> Theme -> Bool -> Bool -> Bool -> PlotBox
computePlotBox sz theme hasTitle hasRightLegend hasBottomLegend =
    let (sceneW, sceneH) = case sz of
            SizeChars w h ->
                (fromIntegral w * themePxPerChar theme, fromIntegral h * themePxPerLine theme)
            SizePixels w h -> (fromIntegral w, fromIntegral h)
            SizeResponsive ar ->
                let h = 320
                 in (h * ar, h)
        topMargin = if hasTitle then themeTitleSize theme + 20 else 10
        bottomMargin = (if hasBottomLegend then 30 else 0) + themeFontSize theme * 1.8 + 8
        leftMargin = themeFontSize theme * 5
        rightMargin = if hasRightLegend then 120 else 20
        plotW = max 1 (sceneW - leftMargin - rightMargin)
        plotH = max 1 (sceneH - topMargin - bottomMargin)
     in PlotBox sceneW sceneH leftMargin topMargin plotW plotH

axesMarks :: Theme -> PlotBox -> Coord -> TrainedScale -> TrainedScale -> [Mark]
axesMarks theme box coord xs ys = case coord of
    CoordCartesian -> cartesianAxes theme box False xs ys
    -- CoordFlip rotates 90° CW so the Y axis grows top-down.
    CoordFlip -> cartesianAxes theme box True ys xs
    CoordPolar aes a0 dir -> polarAxes theme box aes a0 dir xs ys

{- | Axes are 'MAxisLine' spines (clean box-drawing chars in terminal,
@<line>@ in SVG). Gridlines and tick stubs are omitted so the
terminal output stays uncluttered; tick /labels/ sit beside each
break. @flipY@ inverts Y for 'CoordFlip'.
-}
cartesianAxes ::
    Theme -> PlotBox -> Bool -> TrainedScale -> TrainedScale -> [Mark]
cartesianAxes theme box flipY xs ys =
    let axisColor = colorOfSpec (themeAxisColor theme)
        textColor = colorOfSpec (themeTextColor theme)

        px = boxX box
        py = boxY box
        pw = boxW box
        ph = boxH box

        spineStyle = defaultStyle{styleStroke = Just axisColor, styleStrokeWidth = 1}

        xAxis = MAxisLine (Point px (py + ph)) (Point (px + pw) (py + ph)) spineStyle
        yAxis = MAxisLine (Point px py) (Point px (py + ph)) spineStyle

        xLabels =
            [ MText
                (Point xPos (py + ph + themeFontSize theme + 4))
                lbl
                defaultTextStyle
                    { textFill = textColor
                    , textSize = themeFontSize theme
                    , textAnchor = AnchorMiddle
                    }
            | (v, lbl) <- zip (tsBreaks xs) (tsLabels xs)
            , inDomain (tsDomain xs) v
            , let xPos = px + tsProject xs v * pw
            ]

        yLabels =
            [ MText
                (Point (px - 6) (yPos + themeFontSize theme / 3))
                lbl
                defaultTextStyle
                    { textFill = textColor
                    , textSize = themeFontSize theme
                    , textAnchor = AnchorEnd
                    }
            | (v, lbl) <- zip (tsBreaks ys) (tsLabels ys)
            , inDomain (tsDomain ys) v
            , let yPos =
                    if flipY
                        then py + tsProject ys v * ph
                        else py + ph - tsProject ys v * ph
            ]
     in [xAxis, yAxis] <> xLabels <> yLabels

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
