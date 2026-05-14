{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Format
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Apply a declarative 'Formatter' to a 'Double'. Replaces the
function-typed @LabelFormatter@ used by the legacy 'Granite.Plot'
record, so the entire chart spec can stay JSON-shaped.
-}
module Granite.Format (
    Formatter (..),
    runFormatter,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showEFloat, showFFloat)

{- | A declarative formatter applied to numeric tick / label values.

The /Default/ behaves like the legacy formatter: scientific for
very small or very large magnitudes, fixed-point with one decimal
otherwise. The other constructors give the caller explicit control
without needing to embed a Haskell function in the spec.
-}
data Formatter
    = FormatDefault
    | -- | fixed point with the given decimals
      FormatPrecision Int
    | -- | scientific notation with the given decimals
      FormatScientific Int
    | -- | percentage (multiplies by 100), with given decimals
      FormatPercent Int
    | -- | integer with comma grouping (1,234,567)
      FormatComma
    | {- | epoch-millisecond value formatted with the given strftime-style
      pattern; unsupported in this phase, returns the raw value.
      -}
      FormatDateTime !Text
    | {- | a printf-like template with a single \"{}" placeholder.
      This phase only supports \"{}" itself.
      -}
      FormatTemplate !Text
    | -- | SI suffixes (1.2k, 3.4M)
      FormatSI
    deriving (Eq, Show, Read)

runFormatter :: Formatter -> Double -> Text
runFormatter f v = case f of
    FormatDefault -> defaultFmt v
    FormatPrecision n -> Text.pack (showFFloat (Just n) v "")
    FormatScientific n -> Text.pack (showEFloat (Just n) v "")
    FormatPercent n -> Text.pack (showFFloat (Just n) (v * 100) "") <> "%"
    FormatComma -> commaGroup (round v :: Integer)
    FormatDateTime _ -> Text.pack (show v)
    FormatTemplate t -> Text.replace "{}" (defaultFmt v) t
    FormatSI -> siSuffix v

defaultFmt :: Double -> Text
defaultFmt v
    | abs v >= 10000 || abs v < 0.01 && v /= 0 =
        Text.pack (showEFloat (Just 1) v "")
    | otherwise = Text.pack (showFFloat (Just 1) v "")

commaGroup :: Integer -> Text
commaGroup n =
    let neg = n < 0
        digits = reverse (show (abs n))
        chunks = chunkN 3 digits
        grouped = reverse (intercalateRev "," chunks)
     in (if neg then "-" else "") <> Text.pack grouped

chunkN :: Int -> [a] -> [[a]]
chunkN _ [] = []
chunkN n xs = take n xs : chunkN n (drop n xs)

intercalateRev :: [a] -> [[a]] -> [a]
intercalateRev sep = go
  where
    go [] = []
    go [x] = x
    go (x : xs) = x ++ sep ++ go xs

siSuffix :: Double -> Text
siSuffix v
    | a >= 1e12 = fmt (v / 1e12) "T"
    | a >= 1e9 = fmt (v / 1e9) "G"
    | a >= 1e6 = fmt (v / 1e6) "M"
    | a >= 1e3 = fmt (v / 1e3) "k"
    | a >= 1 = Text.pack (showFFloat (Just 1) v "")
    | a >= 1e-3 = fmt (v * 1e3) "m"
    | a >= 1e-6 = fmt (v * 1e6) "\x00b5"
    | otherwise = Text.pack (showEFloat (Just 1) v "")
  where
    a = abs v
    fmt x suf = Text.pack (showFFloat (Just 1) x "") <> suf
