{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Internal.Util
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Internal helpers shared across backends.
-}
module Granite.Internal.Util (
    clamp,
    eps,
    minimum',
    maximum',
    mod',
    angleWithin,
    quartiles,
    normalize,
    setAt,
    updateAt,
    addAt,
    gridWidth,
    wcswidth,
    ellipsisize,
    estLabelWidthPx,
    estMaxGlyphs,
    truncatePx,
    justifyRight,
    showD,
    escXml,
    ticks1D,
) where

import Data.List qualified as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showFFloat)

clamp :: (Ord a) => a -> a -> a -> a
clamp low high x = max low (min high x)

eps :: Double
eps = 1e-12

minimum', maximum' :: [Double] -> Double
minimum' [] = 0
minimum' xs = minimum xs
maximum' [] = 1
maximum' xs = maximum xs

mod' :: Double -> Double -> Double
mod' a m = a - fromIntegral (floor (a / m) :: Int) * m

angleWithin :: Double -> Double -> Double -> Bool
angleWithin ang a0 a1
    | a1 >= a0 = ang >= a0 && ang <= a1
    | otherwise = ang >= a0 || ang <= a1

{- | Five-number summary: @(min, Q1, median, Q3, max)@. For @n < 5@,
all five values collapse to the mean.
-}
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

setAt :: [a] -> Int -> a -> [a]
setAt xs i v = updateAt xs i (const v)

updateAt :: [a] -> Int -> (a -> a) -> [a]
updateAt xs i f
    | i < 0 = xs
    | otherwise = go xs i
  where
    go [] _ = []
    go (x : rest) 0 = f x : rest
    go (x : rest) n = x : go rest (n - 1)

addAt :: [Int] -> Int -> Int -> [Int]
addAt xs i v = updateAt xs i (+ v)

gridWidth :: [[a]] -> Int
gridWidth [] = 0
gridWidth (x : _) = length x

-- | Visible width of text, skipping ANSI escape sequences.
wcswidth :: Text -> Int
wcswidth = go 0
  where
    go acc xs
        | Text.null xs = acc
        | Text.isPrefixOf "\ESC[" xs =
            let rest' = Text.dropWhile (/= 'm') xs
             in if Text.null rest' then acc else go acc (Text.tail rest')
        | otherwise = go (acc + 1) (Text.tail xs)

{- | Ensure the text fits within maxWidth. If it doesn't, truncate and append an ellipsis.
>>> ellipsisize 5 "Hello, World!"
"Hell\8230"
>>> ellipsisize 1 "Hi"
"\8230"
>>> ellipsisize 0 "Hello, World!"
""
>>> ellipsisize 20 "Hello, World!"
"Hello, World!"
-}
ellipsisize :: Int -> Text -> Text
ellipsisize maxWidth lbl
    | maxWidth <= 0 = ""
    | wcswidth lbl > maxWidth = Text.take (maxWidth - 1) lbl <> "…"
    | otherwise = lbl

-- | Average glyph advance as a fraction of the em, for proportional fonts.
glyphAspectEm :: Double
glyphAspectEm = 0.6

{- | Rough rendered pixel width of a label at a given font size: visible
glyph count times 'glyphAspectEm'. Good enough to decide when axis labels
would collide.
-}
estLabelWidthPx :: Double -> Text -> Double
estLabelWidthPx fontSize t = fromIntegral (wcswidth t) * fontSize * glyphAspectEm

{- | Inverse of 'estLabelWidthPx': the most glyphs that fit within @limitPx@
at the given font size. Pair with 'ellipsisize' to truncate to a pixel budget.
-}
estMaxGlyphs :: Double -> Double -> Int
estMaxGlyphs fontSize limitPx = floor (limitPx / (fontSize * glyphAspectEm))

{- | Fit @full@ within @limitPx@ at the given font size: returns the
(possibly ellipsised) label and, when it was shortened, the full text for a
tooltip. Used wherever a label must not overrun its slot (axis ticks, facet
strips).
-}
truncatePx :: Double -> Double -> Text -> (Text, Maybe Text)
truncatePx fontSize limitPx full
    | estLabelWidthPx fontSize full <= limitPx = (full, Nothing)
    | otherwise =
        (ellipsisize (max 1 (estMaxGlyphs fontSize limitPx)) full, Just full)

justifyRight :: Int -> Text -> Text
justifyRight n s = Text.replicate (max 0 (n - wcswidth s)) " " <> s

showD :: Double -> Text
showD d
    | d == fromIntegral (round d :: Int) = Text.pack (show (round d :: Int))
    | otherwise = Text.pack (showFFloat (Just 2) d "")

escXml :: Text -> Text
escXml =
    Text.replace "&" "&amp;"
        . Text.replace "<" "&lt;"
        . Text.replace ">" "&gt;"
        . Text.replace "\"" "&quot;"

{- | Evenly spaced tick positions in screen space paired with data values.
With @invertY = True@, position 0 maps to @vmax@ (top row).
-}
ticks1D :: Int -> Int -> (Double, Double) -> Bool -> [(Int, Double)]
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
