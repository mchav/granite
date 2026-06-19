{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Render.Svg
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

SVG backend.
-}
module Granite.Render.Svg (
    renderScene,
    renderSceneResponsive,
    attr,
    svgDoc,
    svgDocWith,
    svgRect,
    svgCircle,
    svgLine,
    svgPolyline,
    svgPath,
    svgText,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Granite.Color (colorHex)
import Granite.Internal.Util (escXml, showD)
import Granite.Render.Scene (
    Mark (..),
    Point (..),
    Rect (..),
    Scene (..),
    Style (..),
    TextAnchor (..),
    TextStyle (..),
 )

attr :: Text -> Text -> Text
attr k v = " " <> k <> "=\"" <> v <> "\""

svgDoc :: Double -> Double -> Text -> Text
svgDoc = svgDocWith False

svgDocWith :: Bool -> Double -> Double -> Text -> Text
svgDocWith responsive w h content =
    "<svg xmlns=\"http://www.w3.org/2000/svg\""
        <> attr "viewBox" ("0 0 " <> showD w <> " " <> showD h)
        <> sizing
        <> attr "font-family" "system-ui, -apple-system, sans-serif"
        <> ">\n"
        <> "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"
        <> content
        <> "</svg>\n"
  where
    sizing
        | responsive =
            attr "preserveAspectRatio" "xMidYMid meet"
                <> attr "width" "100%"
                <> attr "style" "height:auto"
        | otherwise =
            attr "width" (showD w) <> attr "height" (showD h)

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

-- | Simple text element (legacy callers); no rotation or tooltip.
svgText :: Double -> Double -> Text -> Text -> Double -> Text -> Text
svgText x y anchor fill size = svgTextRT x y anchor fill size 0 Nothing

{- | Text element with optional rotation (degrees about @(x,y)@) and an
optional @\<title\>@ tooltip (the untruncated label). @rot = 0@ and
@title = Nothing@ produce byte-identical output to 'svgText'.
-}
svgTextRT ::
    Double ->
    Double ->
    Text ->
    Text ->
    Double ->
    Double ->
    Maybe Text ->
    Text ->
    Text
svgTextRT x y anchor fill size rot mTitle content =
    "<text"
        <> attr "x" (showD x)
        <> attr "y" (showD y)
        <> attr "text-anchor" anchor
        <> attr "fill" fill
        <> attr "font-size" (showD size)
        <> rotAttr
        <> ">"
        <> titleEl
        <> escXml content
        <> "</text>\n"
  where
    rotAttr
        | rot == 0 = ""
        | otherwise =
            attr
                "transform"
                ("rotate(" <> showD rot <> " " <> showD x <> " " <> showD y <> ")")
    titleEl = case mTitle of
        Nothing -> ""
        Just t -> "<title>" <> escXml t <> "</title>"

svgPath :: Text -> Text -> Text -> Text
svgPath d fill extra =
    "<path"
        <> attr "d" d
        <> attr "fill" fill
        <> extra
        <> "/>\n"

renderScene :: Scene -> Text
renderScene scene =
    svgDoc (sceneWidth scene) (sceneHeight scene) $
        T.concat (map renderMark (sceneMarks scene))

renderSceneResponsive :: Scene -> Text
renderSceneResponsive scene =
    svgDocWith True (sceneWidth scene) (sceneHeight scene) $
        T.concat (map renderMark (sceneMarks scene))

renderMark :: Mark -> Text
renderMark m = case m of
    MRect (Rect x y w h) sty ->
        let fill = maybe "none" colorHex (styleFill sty)
            extras =
                strokeAttrs sty
                    <> opacityAttr sty
         in svgRect x y w h fill extras
    MCircle (Point x y) r sty ->
        let fill = maybe "none" colorHex (styleFill sty)
         in "<circle"
                <> attr "cx" (showD x)
                <> attr "cy" (showD y)
                <> attr "r" (showD r)
                <> attr "fill" fill
                <> strokeAttrs sty
                <> opacityAttr sty
                <> "/>\n"
    MText (Point x y) txt ts ->
        svgTextRT
            x
            y
            (anchorText (textAnchor ts))
            (colorHex (textFill ts))
            (textSize ts)
            (textRotate ts)
            (textTitle ts)
            txt
    MPolyline pts sty ->
        let stroke = maybe "#000000" colorHex (styleStroke sty)
            pairs = [(px, py) | Point px py <- pts]
         in svgPolyline pairs stroke (styleStrokeWidth sty)
    MPolygon pts sty ->
        let fill = maybe "none" colorHex (styleFill sty)
            pairs = [(px, py) | Point px py <- pts]
            ptsStr = T.intercalate " " [showD px <> "," <> showD py | (px, py) <- pairs]
         in "<polygon"
                <> attr "points" ptsStr
                <> attr "fill" fill
                <> strokeAttrs sty
                <> opacityAttr sty
                <> "/>\n"
    MAxisLine (Point x1 y1) (Point x2 y2) sty ->
        let stroke = maybe "#000000" colorHex (styleStroke sty)
         in svgLine x1 y1 x2 y2 stroke (styleStrokeWidth sty)
    MPath d sty ->
        let fill = maybe "none" colorHex (styleFill sty)
         in svgPath d fill (strokeAttrs sty)
    MArc (Point cx cy) r a0 a1 sty ->
        let x0 = cx + r * cos a0
            y0 = cy + r * sin a0
            x1 = cx + r * cos a1
            y1 = cy + r * sin a1
            largeArc = if a1 - a0 > pi then "1" else "0"
            d =
                "M "
                    <> showD cx
                    <> " "
                    <> showD cy
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
            fill = maybe "none" colorHex (styleFill sty)
         in svgPath d fill (strokeAttrs sty)
    MGroup ms ->
        "<g>" <> T.concat (map renderMark ms) <> "</g>\n"

anchorText :: TextAnchor -> Text
anchorText AnchorStart = "start"
anchorText AnchorMiddle = "middle"
anchorText AnchorEnd = "end"

strokeAttrs :: Style -> Text
strokeAttrs sty =
    case styleStroke sty of
        Nothing -> ""
        Just c ->
            attr "stroke" (colorHex c) <> attr "stroke-width" (showD (styleStrokeWidth sty))

opacityAttr :: Style -> Text
opacityAttr sty
    | styleFillOpacity sty < 1 = attr "fill-opacity" (showD (styleFillOpacity sty))
    | otherwise = ""
