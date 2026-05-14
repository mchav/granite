
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Scale
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Scale training: turn a declarative 'Scale' into a 'TrainedScale' that
projects data values onto [0,1] and generates round-number ticks.
-}
module Granite.Scale (
    TrainedScale (..),
    train,
    niceTicks,
    niceNum,
) where

import Data.Text (Text)

import Granite.Format (Formatter (..), runFormatter)
import Granite.Internal.Util (eps)
import Granite.Spec (
    BreaksSpec (..),
    Expand (..),
    LogBase (..),
    Scale (..),
    ScaleOpts (..),
 )

data TrainedScale = TrainedScale
    { tsDomain :: !(Double, Double)
    , tsProject :: !(Double -> Double)
    , tsUnproject :: !(Double -> Double)
    , tsBreaks :: ![Double]
    , tsLabels :: ![Text]
    }

train :: Scale -> (Double, Double) -> TrainedScale
train scale dataRange = case scale of
    SLinear opts -> trainLinear opts dataRange
    SLog base opts -> trainLog base opts dataRange
    SSqrt opts -> trainSqrt opts dataRange
    SIdentity -> trainIdentity dataRange
    SReverse inner -> reverseScale (train inner dataRange)
    SDiscrete -> trainLinear (defaultOpts BreaksNice) dataRange
    SColorContinuous _ -> trainLinear (defaultOpts BreaksNice) dataRange
    SColorDiscrete _ -> trainLinear (defaultOpts BreaksNice) dataRange

defaultOpts :: BreaksSpec -> ScaleOpts
defaultOpts brk =
    ScaleOpts
        { scaleDomain = Nothing
        , scaleBreaks = brk
        , scaleLabels = FormatDefault
        , scaleExpand = Expand 0.05 0
        , scaleClip = False
        }

trainLinear :: ScaleOpts -> (Double, Double) -> TrainedScale
trainLinear opts dataRange =
    let (lo, hi) = expandRange (scaleExpand opts) (overrideRange (scaleDomain opts) dataRange)
        span_ = hi - lo + eps
        project v = (v - lo) / span_
        unproject t = lo + t * span_
        breaks = chooseBreaks (scaleBreaks opts) (lo, hi)
        labels = map (runFormatter (scaleLabels opts)) breaks
     in TrainedScale (lo, hi) project unproject breaks labels

{- | Log scales need a strictly positive domain. When the data range
contains zero or negatives we fall back to a window 3 decades below
the data max (or [1, 10] if neither bound is positive).
-}
trainLog :: LogBase -> ScaleOpts -> (Double, Double) -> TrainedScale
trainLog base opts (dlo, dhi) =
    let safeLo
            | dlo > 0 = dlo
            | dhi > 0 = dhi / 1000
            | otherwise = 1
        safeHi
            | dhi > safeLo = dhi
            | otherwise = safeLo * 10
        (lo, hi) =
            expandLog (scaleExpand opts) (overrideRange (scaleDomain opts) (safeLo, safeHi))
        b = logBaseConst base
        ll = logBase b lo
        lh = logBase b hi
        span_ = lh - ll + eps
        project v = (logBase b (max safeLo v) - ll) / span_
        unproject t = b ** (ll + t * span_)
        breaks = case scaleBreaks opts of
            BreaksAt xs -> xs
            BreaksCount n -> sampleLogBreaks b lo hi n
            BreaksNice -> integerPowers b lo hi
        labels = map (runFormatter (scaleLabels opts)) breaks
     in TrainedScale (lo, hi) project unproject breaks labels

logBaseConst :: LogBase -> Double
logBaseConst Base2 = 2
logBaseConst BaseE = exp 1
logBaseConst Base10 = 10

integerPowers :: Double -> Double -> Double -> [Double]
integerPowers b lo hi =
    let kLo = floor (logBase b lo) :: Int
        kHi = ceiling (logBase b hi) :: Int
        n = kHi - kLo + 1
        maxTicks = 10
     in if n <= maxTicks
            then [b ** fromIntegral k | k <- [kLo .. kHi]]
            else
                let stride = (n + maxTicks - 1) `div` maxTicks
                 in [b ** fromIntegral k | k <- [kLo, kLo + stride .. kHi]]

sampleLogBreaks :: Double -> Double -> Double -> Int -> [Double]
sampleLogBreaks b lo hi n
    | n < 2 = [lo, hi]
    | otherwise =
        let ll = logBase b lo
            lh = logBase b hi
            step = (lh - ll) / fromIntegral (n - 1)
         in [b ** (ll + step * fromIntegral i) | i <- [0 .. n - 1]]

trainSqrt :: ScaleOpts -> (Double, Double) -> TrainedScale
trainSqrt opts dataRange =
    let (lo0, hi0) = overrideRange (scaleDomain opts) dataRange
        lo = max 0 lo0
        hi = max lo hi0
        (lo', hi') = expandRange (scaleExpand opts) (lo, hi)
        sLo = sqrt (max 0 lo')
        sHi = sqrt (max sLo hi')
        span_ = sHi - sLo + eps
        project v = (sqrt (max 0 v) - sLo) / span_
        unproject t =
            let s = sLo + t * span_
             in s * s
        breaks = chooseBreaks (scaleBreaks opts) (lo', hi')
        labels = map (runFormatter (scaleLabels opts)) breaks
     in TrainedScale (lo', hi') project unproject breaks labels

trainIdentity :: (Double, Double) -> TrainedScale
trainIdentity (lo, hi) =
    let breaks = chooseBreaks BreaksNice (lo, hi)
        labels = map (runFormatter FormatDefault) breaks
     in TrainedScale
            { tsDomain = (lo, hi)
            , tsProject = id
            , tsUnproject = id
            , tsBreaks = breaks
            , tsLabels = labels
            }

reverseScale :: TrainedScale -> TrainedScale
reverseScale ts =
    ts
        { tsProject = \v -> 1 - tsProject ts v
        , tsUnproject = tsUnproject ts . (1 -)
        }

overrideRange :: Maybe (Double, Double) -> (Double, Double) -> (Double, Double)
overrideRange Nothing r = r
overrideRange (Just (a, b)) _ = (a, b)

expandRange :: Expand -> (Double, Double) -> (Double, Double)
expandRange (Expand m a) (lo, hi) =
    let pad = (hi - lo) * m + a
     in (lo - pad, hi + pad)

expandLog :: Expand -> (Double, Double) -> (Double, Double)
expandLog (Expand m _) (lo, hi)
    | m <= 0 = (lo, hi)
    | otherwise =
        let factor = (hi / lo) ** m
         in (lo / factor, hi * factor)

chooseBreaks :: BreaksSpec -> (Double, Double) -> [Double]
chooseBreaks brk (lo, hi) = case brk of
    BreaksAt xs -> xs
    BreaksCount n -> niceTicks (lo, hi) n
    BreaksNice -> niceTicks (lo, hi) 5

{- | Heckbert "loose label" tick selection. Round-number positions in
@{1, 2, 2.5, 5} × 10^k@ that bracket the data range.
-}
niceTicks :: (Double, Double) -> Int -> [Double]
niceTicks (lo, hi) target0
    | not (isValid lo && isValid hi) || lo == hi = [lo]
    | lo > hi = niceTicks (hi, lo) target0
    | otherwise =
        let target = max 2 target0
            range = niceNum (hi - lo) False
            step = niceNum (range / fromIntegral (target - 1)) True
            gMin = (fromIntegral :: Int -> Double) (floor (lo / step)) * step
            gMax = (fromIntegral :: Int -> Double) (ceiling (hi / step)) * step
            n = round ((gMax - gMin) / step) + 1
         in [gMin + step * fromIntegral i | i <- [0 .. n - 1]]
  where
    isValid x = not (isNaN x || isInfinite x)

niceNum :: Double -> Bool -> Double
niceNum 0 _ = 1
niceNum x roundIt =
    let absX = abs x
        sign = if x < 0 then (-1) else 1
        exp10 = floor (logBase 10 absX) :: Int
        f = absX / (10 ** fromIntegral exp10)
        nf
            | roundIt =
                if f < 1.5
                    then 1
                    else
                        if f < 3
                            then 2
                            else
                                if f < 7
                                    then 5
                                    else 10
            | f <= 1 = 1
            | f <= 2 = 2
            | f <= 5 = 5
            | otherwise = 10
     in sign * nf * (10 ** fromIntegral exp10)
