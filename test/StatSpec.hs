{-# LANGUAGE OverloadedStrings #-}

module StatSpec (spec) where

import Granite.Spec (BinSpec (..), SummaryFun (..))
import Granite.Stat (
    BoxStats (..),
    binData,
    boxplotSummary,
    countByCategory,
    kdeData,
    smoothLm,
    smoothMovingAvg,
    summarizeByCategory,
 )
import Test.Hspec

spec :: Spec
spec = describe "Granite.Stat" $ do
    describe "binData" $ do
        it "10 evenly-spaced values into 5 equal-width bins → counts of 2 each" $ do
            let xs = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5]
                (mids, counts) = binData (BinByCount 5) xs
            length mids `shouldBe` 5
            counts `shouldBe` [2, 2, 2, 2, 2]

        it "BinByWidth covers every input value" $ do
            let (_, counts) = binData (BinByWidth 1.0) [0, 0.5, 1.2, 1.8, 2.1]
            sum counts `shouldBe` 5

        it "BinByEdges respects manual breaks" $ do
            let (_, counts) = binData (BinByEdges [0, 1, 2, 3]) [0.5, 1.5, 2.5, 2.7]
            counts `shouldBe` [1, 1, 2]

        it "empty input → empty output" $
            binData (BinByCount 5) [] `shouldBe` ([], [])

    describe "kdeData" $ do
        it "produces n grid points" $ do
            let (gx, gy) = kdeData 64 [0, 1, 2, 3, 4, 5]
            length gx `shouldBe` 64
            length gy `shouldBe` 64

        it "all density values are non-negative" $ do
            let (_, gy) = kdeData 32 [0, 1, 2, 3, 4, 5]
            all (>= 0) gy `shouldBe` True

    describe "smoothLm" $ do
        it "fits a perfect line exactly" $ do
            let xs = [0, 1, 2, 3, 4]
                ys = [1, 3, 5, 7, 9]
                fitted = smoothLm xs ys
            all (< 1e-9) (zipWith (\a b -> abs (a - b)) ys fitted) `shouldBe` True

    describe "smoothMovingAvg" $ do
        it "window of 1 is identity" $
            smoothMovingAvg 1 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]

        it "window of 3 on a constant signal preserves the constant" $
            all (== 3) (smoothMovingAvg 3 [3, 3, 3, 3, 3]) `shouldBe` True

    describe "boxplotSummary" $ do
        it "[1..9] → min=1, Q1=3, med=5, Q3=7, max=9" $ do
            let bs = boxplotSummary [1 .. 9]
            yMin bs `shouldBe` 1
            q1 bs `shouldBe` 3
            med bs `shouldBe` 5
            q3 bs `shouldBe` 7
            yMax bs `shouldBe` 9

    describe "countByCategory" $ do
        it "preserves first-seen order" $
            countByCategory ["b", "a", "b", "c", "a"]
                `shouldBe` [("b", 2), ("a", 2), ("c", 1)]

    describe "summarizeByCategory" $ do
        it "mean per group" $
            summarizeByCategory SumMean [("a", 1), ("a", 3), ("b", 10), ("b", 20)]
                `shouldBe` [("a", 2.0), ("b", 15.0)]

        it "sum per group" $
            summarizeByCategory SumSum [("a", 1), ("a", 3), ("b", 10), ("b", 20)]
                `shouldBe` [("a", 4.0), ("b", 30.0)]

        it "median per group" $
            summarizeByCategory
                SumMedian
                [("a", 1), ("a", 2), ("a", 3), ("b", 10), ("b", 20), ("b", 30)]
                `shouldBe` [("a", 2.0), ("b", 20.0)]
