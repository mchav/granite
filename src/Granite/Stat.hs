{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Stat
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Statistical transforms ('Stat'). Each runs on a layer's mapped X / Y
columns and returns a new frame, generally with the same column names
holding the transformed values. 'Granite.Spec.StatBoxplot' additionally writes
@__ymin@ / @__q1@ / @__median@ / @__q3@ / @__ymax@ for the boxplot
geom to read.
-}
module Granite.Stat (
    applyStat,
    binData,
    kdeData,
    smoothLm,
    smoothMovingAvg,
    smoothLoess,
    boxplotSummary,
    BoxStats (..),
    countByCategory,
    summarizeByCategory,
) where

import Data.List qualified as List
import Data.Text (Text)

import Granite.Data.Frame (
    Column (..),
    DataFrame (..),
    columnAsNum,
    columnAsText,
    fromColumns,
    lookupColumn,
 )
import Granite.Spec (
    BinSpec (..),
    ColumnRef (..),
    Mapping (..),
    SmoothMethod (..),
    Stat (..),
    SummaryFun (..),
 )

applyStat :: Stat -> Mapping -> DataFrame -> DataFrame
applyStat stat m df = case stat of
    StatIdentity -> df
    StatBin spec -> runBin spec m df
    StatDensity -> runDensity m df
    StatSmooth method -> runSmooth method m df
    StatBoxplot -> runBoxplot m df
    StatCount -> runCount m df
    StatSummary fn -> runSummary fn m df

runBin :: BinSpec -> Mapping -> DataFrame -> DataFrame
runBin spec m df =
    case resolveNum (aesX m) df of
        Nothing -> df
        Just xs ->
            let (mids, counts) = binData spec xs
                xName = refName (aesX m) "x"
                yName = refName (aesY m) "count"
             in fromColumns
                    [ (xName, ColNum mids)
                    , (yName, ColNum counts)
                    ]

binData :: BinSpec -> [Double] -> ([Double], [Double])
binData _ [] = ([], [])
binData spec xs =
    let lo = minimum xs
        hi = maximum xs
        edges = case spec of
            BinByCount n -> evenEdges n lo hi
            BinByWidth w
                | w <= 0 -> evenEdges 1 lo hi
                | otherwise -> stepEdges w lo hi
            BinByEdges es -> List.sort es
        nBins = max 1 (length edges - 1)
        mids =
            [ (e0 + e1) / 2
            | (e0, e1) <- zip edges (drop 1 edges)
            ]
        counts0 = replicate nBins (0 :: Int)
        counts =
            foldr
                ( \v acc ->
                    let ix = which edges v
                     in update acc ix
                )
                counts0
                xs
     in (mids, map fromIntegral counts)
  where
    update :: [Int] -> Int -> [Int]
    update cs i
        | i < 0 || i >= length cs = cs
        | otherwise = take i cs ++ [cs !! i + 1] ++ drop (i + 1) cs

    -- Epsilon tolerance so values landing exactly on an internal edge
    -- go to the right-side bin, matching @[lo, hi)@ even after the
    -- rounding error in @lo + step * i@.
    which :: [Double] -> Double -> Int
    which es v
        | length es < 2 = -1
        | v + eps < head es = -1
        | v > last es + eps = -1
        | otherwise = go 0
      where
        eps = 1e-9
        n = length es - 1
        go i
            | i >= n = n - 1
            | v + eps < es !! (i + 1) = i
            | i == n - 1 && v <= es !! (i + 1) + eps = i
            | otherwise = go (i + 1)

evenEdges :: Int -> Double -> Double -> [Double]
evenEdges n lo hi
    | n < 1 = [lo, hi]
    | lo == hi = [lo - 0.5, hi + 0.5]
    | otherwise =
        let step = (hi - lo) / fromIntegral n
         in [lo + step * fromIntegral i | i <- [0 .. n]]

stepEdges :: Double -> Double -> Double -> [Double]
stepEdges w lo hi
    | lo == hi = [lo - w / 2, hi + w / 2]
    | otherwise =
        let n = max 1 (ceiling ((hi - lo) / w)) :: Int
         in [lo + w * fromIntegral i | i <- [0 .. n]]

runDensity :: Mapping -> DataFrame -> DataFrame
runDensity m df =
    case resolveNum (aesX m) df of
        Nothing -> df
        Just xs ->
            let (gx, gy) = kdeData 128 xs
                xName = refName (aesX m) "x"
                yName = refName (aesY m) "density"
             in fromColumns [(xName, ColNum gx), (yName, ColNum gy)]

-- | Gaussian KDE on @n@ grid points; bandwidth is Silverman's rule.
kdeData :: Int -> [Double] -> ([Double], [Double])
kdeData _ [] = ([], [])
kdeData n xs =
    let nn = length xs
        mu = sum xs / fromIntegral nn
        var = sum [(x - mu) ** 2 | x <- xs] / fromIntegral (max 1 (nn - 1))
        sigma = sqrt (max var 1e-12)
        h = 1.06 * sigma * fromIntegral nn ** (-0.2)
        lo = minimum xs - 3 * h
        hi = maximum xs + 3 * h
        grid = evenSpaced n lo hi
        density g =
            let k v = exp (negate ((g - v) ** 2) / (2 * h * h)) / (h * sqrt (2 * pi))
             in sum (map k xs) / fromIntegral nn
     in (grid, map density grid)

evenSpaced :: Int -> Double -> Double -> [Double]
evenSpaced n lo hi
    | n < 2 = [lo]
    | otherwise =
        let step = (hi - lo) / fromIntegral (n - 1)
         in [lo + step * fromIntegral i | i <- [0 .. n - 1]]

runSmooth :: SmoothMethod -> Mapping -> DataFrame -> DataFrame
runSmooth method m df =
    case ( resolveNum (aesX m) df
         , resolveNum (aesY m) df
         ) of
        (Just xs, Just ys) ->
            let pairs = List.sortOn fst (zip xs ys)
                (sx, sy) = unzip pairs
                fitted = case method of
                    SmoothLm -> smoothLm sx sy
                    SmoothLoess span_ -> smoothLoess span_ sx sy
                    SmoothMovingAvg w -> smoothMovingAvg w sy
                xName = refName (aesX m) "x"
                yName = refName (aesY m) "y"
             in fromColumns [(xName, ColNum sx), (yName, ColNum fitted)]
        _ -> df

-- | OLS regression, evaluated at the input X positions.
smoothLm :: [Double] -> [Double] -> [Double]
smoothLm xs ys
    | length xs < 2 = ys
    | otherwise =
        let n = fromIntegral (length xs) :: Double
            mx = sum xs / n
            my = sum ys / n
            num = sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys)
            den = sum [(x - mx) ** 2 | x <- xs]
            slope = if den == 0 then 0 else num / den
            intercept = my - slope * mx
         in [intercept + slope * x | x <- xs]

{- | Trailing moving average; leading edge is the partial-window mean
so the output length matches the input.
-}
smoothMovingAvg :: Int -> [Double] -> [Double]
smoothMovingAvg w ys
    | w <= 1 = ys
    | otherwise =
        [ let lo = max 0 (i - w + 1)
              window = take (i - lo + 1) (drop lo ys)
              k = fromIntegral (length window) :: Double
           in if k == 0 then 0 else sum window / k
        | i <- [0 .. length ys - 1]
        ]

{- | LOESS-style local linear regression with tricube weights;
@span_@ is the neighbourhood fraction in @(0, 1]@.
-}
smoothLoess :: Double -> [Double] -> [Double] -> [Double]
smoothLoess span_ xs ys
    | length xs < 2 = ys
    | otherwise =
        let n = length xs
            k = max 2 (round (span_ * fromIntegral n))
         in [loessAt xs ys k x | x <- xs]

loessAt :: [Double] -> [Double] -> Int -> Double -> Double
loessAt xs ys k x0 =
    let dists = [(abs (x - x0), x, y) | (x, y) <- zip xs ys]
        nearest = take k (List.sortOn (\(d, _, _) -> d) dists)
        maxD = case nearest of
            [] -> 1
            ds -> maximum [d | (d, _, _) <- ds] + 1e-12
        wts = [tricube (d / maxD) | (d, _, _) <- nearest]
        xv = [x | (_, x, _) <- nearest]
        yv = [y | (_, _, y) <- nearest]
        sw = sum wts
        swx = sum (zipWith (*) wts xv)
        swy = sum (zipWith (*) wts yv)
        swxx = sum (zipWith (\w x -> w * x * x) wts xv)
        swxy = sum (zipWith3 (\w x y -> w * x * y) wts xv yv)
        denom = sw * swxx - swx * swx
        slope = if denom == 0 then 0 else (sw * swxy - swx * swy) / denom
        intercept = if sw == 0 then 0 else (swy - slope * swx) / sw
     in intercept + slope * x0

tricube :: Double -> Double
tricube u
    | abs u >= 1 = 0
    | otherwise = (1 - abs u ** 3) ** 3

runBoxplot :: Mapping -> DataFrame -> DataFrame
runBoxplot m df =
    case resolveNum (aesY m) df of
        Nothing -> df
        Just ys ->
            let groupNames = case aesX m of
                    Just (ColumnRef n) -> case lookupColumn n df of
                        Just c -> columnAsText c
                        Nothing -> replicate (length ys) "all"
                    Nothing -> replicate (length ys) "all"
                grouped = groupBy fst (zip groupNames ys)
                summaries =
                    [ (name, boxplotSummary [v | (_, v) <- xs])
                    | (name, xs) <- grouped
                    ]
                xName = refName (aesX m) "group"
             in fromColumns
                    [ (xName, ColCat [n | (n, _) <- summaries])
                    , ("__ymin", ColNum [yMin s | (_, s) <- summaries])
                    , ("__q1", ColNum [q1 s | (_, s) <- summaries])
                    , ("__median", ColNum [med s | (_, s) <- summaries])
                    , ("__q3", ColNum [q3 s | (_, s) <- summaries])
                    , ("__ymax", ColNum [yMax s | (_, s) <- summaries])
                    ]

data BoxStats = BoxStats
    { yMin :: !Double
    , q1 :: !Double
    , med :: !Double
    , q3 :: !Double
    , yMax :: !Double
    }
    deriving (Eq, Show)

{- | Five-number summary using Tukey hinges: when @n@ is odd the
median is included in both halves, so @boxplotSummary [1..9]@
gives @(1, 3, 5, 7, 9)@.
-}
boxplotSummary :: [Double] -> BoxStats
boxplotSummary [] = BoxStats 0 0 0 0 0
boxplotSummary xs =
    let sorted = List.sort xs
        n = length sorted
        m =
            if n `mod` 2 == 1
                then sorted !! (n `div` 2)
                else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2
        half =
            if n `mod` 2 == 1
                then (n `div` 2) + 1
                else n `div` 2
        lower = take half sorted
        upper = drop (n - half) sorted
        qq xs' =
            if null xs'
                then m
                else
                    let k = length xs'
                     in if k `mod` 2 == 1
                            then xs' !! (k `div` 2)
                            else (xs' !! (k `div` 2 - 1) + xs' !! (k `div` 2)) / 2
     in BoxStats
            { yMin = head sorted
            , q1 = qq lower
            , med = m
            , q3 = qq upper
            , yMax = last sorted
            }

runCount :: Mapping -> DataFrame -> DataFrame
runCount m df =
    let xs = case aesX m of
            Just (ColumnRef n) -> maybe [] columnAsText (lookupColumn n df)
            Nothing -> []
        counts = countByCategory xs
        xName = refName (aesX m) "x"
        yName = refName (aesY m) "count"
     in fromColumns
            [ (xName, ColCat [k | (k, _) <- counts])
            , (yName, ColNum [fromIntegral v | (_, v) <- counts])
            ]

countByCategory :: [Text] -> [(Text, Int)]
countByCategory xs =
    let uniq = List.nub xs
     in [(u, length [x | x <- xs, x == u]) | u <- uniq]

runSummary :: SummaryFun -> Mapping -> DataFrame -> DataFrame
runSummary fn m df =
    case resolveNum (aesY m) df of
        Nothing -> df
        Just ys ->
            let groupKeys = case aesX m of
                    Just (ColumnRef n) -> case lookupColumn n df of
                        Just c -> columnAsText c
                        Nothing -> replicate (length ys) "all"
                    Nothing -> replicate (length ys) "all"
                summaries = summarizeByCategory fn (zip groupKeys ys)
                xName = refName (aesX m) "x"
                yName = refName (aesY m) "y"
             in fromColumns
                    [ (xName, ColCat [k | (k, _) <- summaries])
                    , (yName, ColNum [v | (_, v) <- summaries])
                    ]

summarizeByCategory :: SummaryFun -> [(Text, Double)] -> [(Text, Double)]
summarizeByCategory fn pairs =
    let grouped = groupBy fst pairs
     in [(name, applyFn fn (map snd vs)) | (name, vs) <- grouped]
  where
    applyFn SumMean xs = if null xs then 0 else sum xs / fromIntegral (length xs)
    applyFn SumSum xs = sum xs
    applyFn SumMedian xs =
        let s = List.sort xs
            n = length s
         in if n == 0
                then 0
                else
                    if n `mod` 2 == 1
                        then s !! (n `div` 2)
                        else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2
    applyFn (SumQuantile p) xs =
        let s = List.sort xs
            n = length s
            ix = max 0 (min (n - 1) (round (p * fromIntegral (n - 1)) :: Int))
         in if n == 0 then 0 else s !! ix

resolveNum :: Maybe ColumnRef -> DataFrame -> Maybe [Double]
resolveNum Nothing _ = Nothing
resolveNum (Just (ColumnRef n)) df = lookupColumn n df >>= columnAsNum

refName :: Maybe ColumnRef -> Text -> Text
refName (Just (ColumnRef n)) _ = n
refName Nothing fallback = fallback

groupBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
groupBy f xs =
    let keys = List.nub (map f xs)
     in [(k, [x | x <- xs, f x == k]) | k <- keys]
