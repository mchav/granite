{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Flame
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Differential flame\/icicle graphs. A 'FlameNode' tree carries, per frame, the
summed positive delta ('fnPos', an increase) and the magnitude of the summed
negative delta ('fnNeg', a decrease, kept non-negative). 'flameDiff' lays the
tree out top→down by depth and emits a standalone @\<svg\>@ document.

A frame's width is its /churn/ (@fnPos + fnNeg@) relative to the root churn.
Within a frame a red part (the increase) and a blue part (the decrease) are
drawn side by side, so a relocation — equal @+x@\/@-x@ — reads as half-red\/
half-blue rather than as a net regression.
-}
module Granite.Flame (
    FlameNode (..),
    FlameOpts (..),
    defFlameOpts,
    flameDiff,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

import Granite.Color (Color (..), colorHex)
import Granite.Internal.Util (escXml, showD, truncatePx)
import Granite.Render.Svg (attr, svgDoc)

data FlameNode = FlameNode
    { fnLabel :: Text
    -- ^ Frame name.
    , fnPos :: Double
    {- ^ Summed positive self-delta in this subtree (an increase), in bytes
    (tooltips format it as MB).
    -}
    , fnNeg :: Double
    {- ^ Summed negative magnitude in this subtree (a decrease, @>= 0@), in bytes
    (tooltips format it as MB).
    -}
    , fnChildren :: [FlameNode]
    }
    deriving (Eq, Show)

data FlameOpts = FlameOpts
    { foWidth :: Double
    -- ^ Total SVG width in px.
    , foRowH :: Double
    -- ^ Height of one depth row in px.
    , foMaxDepth :: Int
    -- ^ Deepest depth drawn (root is depth 0).
    , foTitle :: Text
    -- ^ Heading drawn above the chart.
    , foMinPx :: Double
    -- ^ Frames narrower than this (in px) are dropped.
    }
    deriving (Eq, Show)

defFlameOpts :: FlameOpts
defFlameOpts =
    FlameOpts
        { foWidth = 1200
        , foRowH = 18
        , foMaxDepth = 24
        , foTitle = "Differential flame graph"
        , foMinPx = 1.5
        }

flameRed :: Color
flameRed = Color 206 80 80

flameBlue :: Color
flameBlue = Color 80 100 206

-- | The churn (rendered width unit) of a frame: its red + blue magnitude.
churn :: FlameNode -> Double
churn n = fnPos n + fnNeg n

{- | Render a differential flame graph as a full standalone @\<svg\>@ document.
Rows go top→down by depth; a frame's width is proportional to its churn over the
root churn; children are laid left→right sorted by descending churn; frames
narrower than 'foMinPx' are dropped and recursion stops past 'foMaxDepth'.
-}
flameDiff :: FlameNode -> FlameOpts -> Text
flameDiff root opts =
    let titleH = if T.null (foTitle opts) then 0 else 24
        body = layout opts root 0 0 (foWidth opts) titleH
        depthUsed = maxDepthDrawn opts root 0 (foWidth opts)
        svgH = titleH + fromIntegral (depthUsed + 1) * foRowH opts + 4
        heading
            | T.null (foTitle opts) = ""
            | otherwise =
                "<text"
                    <> attr "x" (showD 8)
                    <> attr "y" (showD 16)
                    <> attr "font-size" (showD 13)
                    <> attr "font-weight" "600"
                    <> attr "fill" "#222"
                    <> ">"
                    <> escXml (foTitle opts)
                    <> "</text>\n"
     in svgDoc (foWidth opts) svgH (heading <> body)

{- | Children paired with their pixel width given the parent's width @w@,
left→right in draw order (descending churn). Shared by 'layout' and
'maxDepthDrawn' so the height estimate uses the same widths that are drawn.
-}
childWidths :: FlameNode -> Double -> [(FlameNode, Double)]
childWidths node w
    | total <= 0 = []
    | otherwise =
        [(k, churn k / total * w) | k <- sortOn (Down . churn) (fnChildren node)]
  where
    total = churn node

{- | The deepest depth that will actually be drawn, so the SVG height fits the
visible frames rather than 'foMaxDepth'. Measures child widths against the
parent's narrowed width (via 'childWidths'), matching 'layout'.
-}
maxDepthDrawn :: FlameOpts -> FlameNode -> Int -> Double -> Int
maxDepthDrawn opts node depth w
    | depth >= foMaxDepth opts = depth
    | otherwise = case visible of
        [] -> depth
        kids -> maximum (depth : [maxDepthDrawn opts k (depth + 1) kw | (k, kw) <- kids])
  where
    visible = [(k, kw) | (k, kw) <- childWidths node w, kw >= foMinPx opts]

{- | Emit a frame and its children. @x0@ is the frame's left edge in px, @w@ its
width in px; children share the parent's width split by churn proportion.
-}
layout :: FlameOpts -> FlameNode -> Int -> Double -> Double -> Double -> Text
layout opts node depth x0 w yOff
    | depth > foMaxDepth opts = ""
    | w < foMinPx opts = ""
    | otherwise = frame <> childMarks
  where
    y = yOff + fromIntegral depth * foRowH opts
    frame = drawFrame opts node x0 y w
    childMarks = T.concat (go x0 (childWidths node w))
    go _ [] = []
    go cx ((k, kw) : ks) =
        layout opts k (depth + 1) cx kw yOff : go (cx + kw) ks

{- | One frame: a red rect (width ∝ 'fnPos') then a blue rect (width ∝ 'fnNeg')
side by side, a thin white stroke, a clipped label when wide enough, and a
@\<title\>@ tooltip carrying the full label plus the pos\/neg in MB.
-}
drawFrame :: FlameOpts -> FlameNode -> Double -> Double -> Double -> Text
drawFrame opts node x y w =
    "<g>" <> tooltip <> redRect <> blueRect <> label <> "</g>\n"
  where
    h = foRowH opts - 1
    total = churn node
    posW = if total <= 0 then 0 else fnPos node / total * w
    negW = w - posW
    redRect = rectAt x y posW h flameRed
    blueRect = rectAt (x + posW) y negW h flameBlue
    tooltip = "<title>" <> escXml titleTxt <> "</title>"
    titleTxt =
        fnLabel node
            <> " (+"
            <> mb (fnPos node)
            <> " / -"
            <> mb (fnNeg node)
            <> ")"
    label
        | w < 24 = ""
        | otherwise =
            let (txt, _) = truncatePx 11 (w - 6) (fnLabel node)
             in "<text"
                    <> attr "x" (showD (x + 3))
                    <> attr "y" (showD (y + h - 4))
                    <> attr "font-size" (showD 11)
                    <> attr "fill" "#fff"
                    <> ">"
                    <> escXml txt
                    <> "</text>\n"

rectAt :: Double -> Double -> Double -> Double -> Color -> Text
rectAt x y w h fill
    | w <= 0 = ""
    | otherwise =
        "<rect"
            <> attr "x" (showD x)
            <> attr "y" (showD y)
            <> attr "width" (showD w)
            <> attr "height" (showD h)
            <> attr "fill" (colorHex fill)
            <> attr "stroke" "#ffffff"
            <> attr "stroke-width" (showD 0.5)
            <> "/>\n"

-- | Format a byte magnitude as megabytes to one decimal place (e.g. @9.4MB@).
mb :: Double -> Text
mb bytes = showD (roundTo 1 (bytes / 1e6)) <> "MB"

-- | Round to @n@ decimal places.
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * f) :: Integer) / f
  where
    f = 10 ^ n
