{-# LANGUAGE OverloadedStrings #-}

module PositionSpec (spec) where

import Data.Text qualified as Text

import Granite.Data.Frame (
    Column (..),
    DataFrame (..),
    fromColumns,
    lookupColumn,
 )
import Granite.Position (applyPosition)
import Granite.Spec (
    ColumnRef (..),
    Mapping (..),
    Position (..),
    emptyMapping,
 )
import Test.Hspec

-- Long-format fixture: two X values, two series ("A", "B"), totals 30
-- at X=1 and 40 at X=2.
sampleFrame :: DataFrame
sampleFrame =
    fromColumns
        [ ("x", ColNum [1, 1, 2, 2])
        , ("y", ColNum [10, 20, 15, 25])
        , ("series", ColCat ["A", "B", "A", "B"])
        ]

sampleMapping :: Mapping
sampleMapping =
    emptyMapping
        { aesX = Just (ColumnRef "x")
        , aesY = Just (ColumnRef "y")
        , aesGroup = Just (ColumnRef "series")
        }

spec :: Spec
spec = describe "Granite.Position" $ do
    describe "PosIdentity" $ do
        it "leaves the frame unchanged" $
            applyPosition PosIdentity sampleMapping sampleFrame
                `shouldBe` sampleFrame

    describe "PosStack" $ do
        let stacked = applyPosition PosStack sampleMapping sampleFrame

        it "produces the same number of rows" $ do
            let DataFrame cols = stacked
            case lookup "x" cols of
                Just (ColNum xs) -> length xs `shouldBe` 4
                _ -> expectationFailure "x column missing"

        it "stacks y values cumulatively within each x" $ do
            case lookupColumn "y" stacked of
                Just (ColNum ys) -> ys `shouldBe` [10, 30, 15, 40]
                _ -> expectationFailure "y column missing"

        it "writes __ybase column with segment bottoms" $ do
            case lookupColumn "__ybase" stacked of
                Just (ColNum bs) -> bs `shouldBe` [0, 10, 0, 15]
                _ -> expectationFailure "__ybase column missing"

    describe "PosFill" $ do
        let filled = applyPosition PosFill sampleMapping sampleFrame

        it "normalises every x-group to sum to 1" $ do
            case lookupColumn "y" filled of
                Just (ColNum ys) ->
                    let near a b = abs (a - b) < 1e-9
                        -- (10/30, (10+20)/30, 15/40, (15+25)/40)
                        expected = [1 / 3, 1, 3 / 8, 1]
                     in and (zipWith near ys expected) `shouldBe` True
                _ -> expectationFailure "y column missing"

    describe "PosDodge" $ do
        let dodged = applyPosition (PosDodge 0.3) sampleMapping sampleFrame

        it "preserves row count" $ do
            case lookupColumn "x" dodged of
                Just (ColNum xs) -> length xs `shouldBe` 4
                _ -> expectationFailure "x column missing"

        it "spreads two series symmetrically around the original x" $ do
            -- nSeries=2, center=0.5, gap=0.3; series A gets -0.15, B gets +0.15
            case lookupColumn "x" dodged of
                Just (ColNum xs) -> xs `shouldBe` [0.85, 1.15, 1.85, 2.15]
                _ -> expectationFailure "x column missing"

    describe "PosJitter" $ do
        let jittered = applyPosition (PosJitter 0.2 0.5) sampleMapping sampleFrame

        it "preserves row count" $ do
            case lookupColumn "x" jittered of
                Just (ColNum xs) -> length xs `shouldBe` 4
                _ -> expectationFailure "x column missing"

        it "is deterministic across runs" $ do
            let again = applyPosition (PosJitter 0.2 0.5) sampleMapping sampleFrame
            jittered `shouldBe` again

        it "keeps offsets within the requested amplitude" $ do
            case (lookupColumn "x" jittered, lookupColumn "y" jittered) of
                (Just (ColNum xs), Just (ColNum ys)) -> do
                    -- amplitude is ±dx and ±dy; allow a tiny epsilon
                    all (\(a, b) -> abs (a - b) <= 0.2 + 1e-9) (zip xs [1, 1, 2, 2])
                        `shouldBe` True
                    all (\(a, b) -> abs (a - b) <= 0.5 + 1e-9) (zip ys [10, 20, 15, 25])
                        `shouldBe` True
                _ -> expectationFailure "x / y missing"

    describe "edge cases" $ do
        it "stack with no group column treats everything as one series" $ do
            let m = emptyMapping{aesX = Just (ColumnRef "x"), aesY = Just (ColumnRef "y")}
                df =
                    fromColumns
                        [ ("x", ColNum [1, 1, 2])
                        , ("y", ColNum [5, 10, 7])
                        ]
                stacked = applyPosition PosStack m df
            case lookupColumn "y" stacked of
                Just (ColNum ys) -> ys `shouldBe` [5, 15, 7]
                _ -> expectationFailure "y missing"

        it "frame without the mapped x/y is returned unchanged for stack" $ do
            let bare = fromColumns [("not_x", ColNum [1, 2, 3])]
                m = emptyMapping{aesX = Just (ColumnRef "x"), aesY = Just (ColumnRef "y")}
            applyPosition PosStack m bare `shouldBe` bare

        it "compiles against the Text import" $
            Text.length "ok" `shouldBe` 2
