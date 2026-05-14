{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Position
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Position adjustments — 'PosStack', 'PosFill', 'PosDodge', 'PosJitter'.
Operates on long-format frames; @stack@ and @fill@ additionally write
a @__ybase@ column for the bar bottom.
-}
module Granite.Position (
    applyPosition,
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
    ColumnRef (..),
    Mapping (..),
    Position (..),
 )

applyPosition :: Position -> Mapping -> DataFrame -> DataFrame
applyPosition pos m df = case pos of
    PosIdentity -> df
    PosStack -> stackPos False m df
    PosFill -> stackPos True m df
    PosDodge gap -> dodgePos gap m df
    PosJitter dx dy -> jitterPos dx dy m df

stackPos :: Bool -> Mapping -> DataFrame -> DataFrame
stackPos fill m df =
    case ( numColAt (aesX m) df
         , numColAt (aesY m) df
         ) of
        (Just xs, Just ys) ->
            let gs = groupKeys m df (length xs)
                rows = zip3 xs ys gs
                uniqXs = List.nub xs
                stackedPerX =
                    concatMap
                        ( \x ->
                            let here = [(x', y', g') | (x', y', g') <- rows, x' == x]
                                ordered = List.sortOn (\(_, _, g) -> seriesIx gs g) here
                                running = scanl (+) 0 [y | (_, y, _) <- ordered]
                                tops = drop 1 running
                                bots = init running
                                segs = zip3 ordered tops bots
                                total = last running
                                norm = if fill && total /= 0 then 1 / total else 1
                             in [(xi, yi * norm, gi, top * norm, bot * norm) | ((xi, yi, gi), top, bot) <- segs]
                        )
                        uniqXs
                xs' = [x | (x, _, _, _, _) <- stackedPerX]
                ys' = [y | (_, _, _, y, _) <- stackedPerX]
                bases = [b | (_, _, _, _, b) <- stackedPerX]
                gs' = [g | (_, _, g, _, _) <- stackedPerX]
                xName = refName (aesX m) "x"
                yName = refName (aesY m) "y"
                gName = case groupRef m of
                    Just (ColumnRef n) -> n
                    Nothing -> "_group"
                base0 = [(xName, ColNum xs'), (yName, ColNum ys'), ("__ybase", ColNum bases)]
                cols =
                    if isJustGroup m
                        then base0 <> [(gName, ColCat gs')]
                        else base0
             in fromColumns cols
        _ -> df

dodgePos :: Double -> Mapping -> DataFrame -> DataFrame
dodgePos gap m df =
    case numColAt (aesX m) df of
        Just xs ->
            let gs = groupKeys m df (length xs)
                seriesOrder = List.nub gs
                nSeries = max 1 (length seriesOrder)
                center = fromIntegral (nSeries - 1) / 2
                shifted =
                    [ x + gap * (fromIntegral (seriesIx gs g) - center)
                    | (x, g) <- zip xs gs
                    ]
                xName = refName (aesX m) "x"
             in replaceColumn xName (ColNum shifted) df
        _ -> df

{- | Deterministic jitter via the golden-ratio low-discrepancy
sequence, so two runs on the same data produce the same offsets
(golden tests stay stable).
-}
jitterPos :: Double -> Double -> Mapping -> DataFrame -> DataFrame
jitterPos dx dy m df =
    let phi = (sqrt 5 - 1) / 2 :: Double
        wobble seed i =
            let r = (fromIntegral i + 1) * phi + seed
             in (r - fromIntegral (floor r :: Int)) - 0.5
        update mc seedScale axis col = case col of
            Just (ColumnRef name) -> case lookupColumn name df of
                Just (ColNum vs) ->
                    let vs' = [v + axis * 2 * wobble seedScale i | (i, v) <- zip [0 :: Int ..] vs]
                     in replaceColumn name (ColNum vs') mc
                _ -> mc
            Nothing -> mc
        df1 = update df 0.123 dx (aesX m)
        df2 = update df1 0.789 dy (aesY m)
     in df2

{- | A 'ColCat' column projects each row to its index in the
unique-value list, so stack / dodge group rows that share a
category.
-}
numColAt :: Maybe ColumnRef -> DataFrame -> Maybe [Double]
numColAt Nothing _ = Nothing
numColAt (Just (ColumnRef n)) df =
    case lookupColumn n df of
        Just (ColCat xs) ->
            let uniques = List.nub xs
                indexOf x = maybe 0 fromIntegral (List.elemIndex x uniques)
             in Just (map indexOf xs)
        Just c -> columnAsNum c
        Nothing -> Nothing

groupRef :: Mapping -> Maybe ColumnRef
groupRef m = case aesGroup m of
    Just r -> Just r
    Nothing -> case aesColor m of
        Just r -> Just r
        Nothing -> aesFill m

isJustGroup :: Mapping -> Bool
isJustGroup m = case groupRef m of
    Just _ -> True
    Nothing -> False

groupKeys :: Mapping -> DataFrame -> Int -> [Text]
groupKeys m df fallbackLen = case groupRef m of
    Just (ColumnRef n) -> case lookupColumn n df of
        Just c -> columnAsText c
        Nothing -> replicate fallbackLen "all"
    Nothing -> replicate fallbackLen "all"

seriesIx :: [Text] -> Text -> Int
seriesIx all_ k =
    let order = List.nub all_
        go _ [] = 0
        go i (x : xs) = if x == k then i else go (i + 1) xs
     in go 0 order

refName :: Maybe ColumnRef -> Text -> Text
refName (Just (ColumnRef n)) _ = n
refName Nothing fb = fb

replaceColumn :: Text -> Column -> DataFrame -> DataFrame
replaceColumn name col (DataFrame cols)
    | any ((== name) . fst) cols =
        DataFrame [(n, if n == name then col else c) | (n, c) <- cols]
    | otherwise = DataFrame (cols <> [(name, col)])
