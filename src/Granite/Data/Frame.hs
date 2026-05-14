{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Data.Frame
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Column-oriented data frame used as input to the chart IR.
-}
module Granite.Data.Frame (
    Column (..),
    columnLength,
    columnAsNum,
    columnAsText,
    DataFrame (..),
    emptyFrame,
    addColumn,
    lookupColumn,
    columnNames,
    frameLength,
    fromColumns,
    filterByRows,
    uniqueText,
    pickRows,
) where

import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

data Column
    = ColNum ![Double]
    | ColCat ![Text]
    | ColTime ![Int64]
    | ColBool ![Bool]
    deriving (Eq, Show, Read)

columnLength :: Column -> Int
columnLength col = case col of
    ColNum xs -> length xs
    ColCat xs -> length xs
    ColTime xs -> length xs
    ColBool xs -> length xs

columnAsNum :: Column -> Maybe [Double]
columnAsNum col = case col of
    ColNum xs -> Just xs
    ColBool xs -> Just (map (\b -> if b then 1 else 0) xs)
    ColTime xs -> Just (map fromIntegral xs)
    ColCat _ -> Nothing

columnAsText :: Column -> [Text]
columnAsText col = case col of
    ColCat xs -> xs
    ColNum xs -> map showD xs
    ColTime xs -> map (Text.pack . show) xs
    ColBool xs -> map (Text.pack . show) xs
  where
    showD :: Double -> Text
    showD d = Text.pack (show d)

newtype DataFrame = DataFrame {unDataFrame :: [(Text, Column)]}
    deriving (Eq, Show, Read)

emptyFrame :: DataFrame
emptyFrame = DataFrame []

addColumn :: Text -> Column -> DataFrame -> DataFrame
addColumn name col (DataFrame xs)
    | any ((== name) . fst) xs =
        DataFrame [(n, if n == name then col else c) | (n, c) <- xs]
    | otherwise = DataFrame (xs ++ [(name, col)])

lookupColumn :: Text -> DataFrame -> Maybe Column
lookupColumn name (DataFrame xs) = lookup name xs

columnNames :: DataFrame -> [Text]
columnNames (DataFrame xs) = map fst xs

frameLength :: DataFrame -> Int
frameLength (DataFrame xs) = foldr (max . columnLength . snd) 0 xs

fromColumns :: [(Text, Column)] -> DataFrame
fromColumns = DataFrame

pickRows :: [Int] -> Column -> Column
pickRows ixs col = case col of
    ColNum xs -> ColNum (pick xs)
    ColCat xs -> ColCat (pick xs)
    ColTime xs -> ColTime (pick xs)
    ColBool xs -> ColBool (pick xs)
  where
    pick :: [a] -> [a]
    pick xs = [xs !! i | i <- ixs, i < length xs]

filterByRows :: [Int] -> DataFrame -> DataFrame
filterByRows ixs (DataFrame cols) =
    DataFrame [(n, pickRows ixs c) | (n, c) <- cols]

uniqueText :: [Text] -> [Text]
uniqueText = List.nub
