{-# LANGUAGE OverloadedStrings #-}

module SpecPropSpec (spec) where

import Data.Text qualified as Text

import Granite.Color (Color (..))
import Granite.Spec
import Test.Hspec

-- A representative chart that exercises every constructor we expect to
-- be JSON-faithful: layers with mappings + stats + positions, scales of
-- various transforms, polar coord, facet wrap, theme, responsive size.
representativeChart :: Chart
representativeChart =
    emptyChart
        { chartData =
            fromColumns
                [ ("x", ColNum [1, 2, 3, 4, 5])
                , ("y", ColNum [1.1, 4.0, 9.2, 16.05, 25.5])
                , ("group", ColCat ["A", "A", "B", "B", "B"])
                ,
                    ( "when"
                    , ColTime
                        [1700000000000, 1700000003600, 1700000007200, 1700000010800, 1700000014400]
                    )
                , ("active", ColBool [True, False, True, False, True])
                ]
        , chartLayers =
            [ (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesColor = Just (ColumnRef "group")
                        }
                , layerAesDef = emptyAesDefaults{defSize = Just 3}
                }
            , (defLayer GeomLine)
                { layerMapping =
                    emptyMapping{aesX = Just (ColumnRef "x"), aesY = Just (ColumnRef "y")}
                , layerStat = StatSmooth (SmoothLoess 0.5)
                }
            , (defLayer GeomBar)
                { layerMapping = emptyMapping{aesX = Just (ColumnRef "group")}
                , layerStat = StatCount
                , layerPosition = PosDodge 0.1
                }
            ]
        , chartScales =
            defScales
                { scaleX = SLog Base10 defScaleOpts{scaleBreaks = BreaksCount 5}
                , scaleY = SSqrt defScaleOpts{scaleLabels = FormatPrecision 2}
                , scaleColor =
                    Just (SColorDiscrete [NamedColor BrightBlue, Hex "#ff0000", RGB 0 128 0])
                }
        , chartCoord = CoordPolar ThetaX 0 PolarCCW
        , chartFacet = FacetWrap (ColumnRef "group") (Just 2) Nothing ScalesFreeY
        , chartTitle = Just "Round-trip fixture"
        , chartSize = SizeResponsive 1.5
        }

spec :: Spec
spec = describe "Granite.Spec" $ do
    describe "show / read round trip (sentinel for JSON faithfulness)" $ do
        it "round-trips emptyChart" $ do
            (read (show emptyChart) :: Chart) `shouldBe` emptyChart

        it "round-trips a representative chart" $ do
            (read (show representativeChart) :: Chart) `shouldBe` representativeChart

        it "double-encoding is idempotent" $ do
            let s = show representativeChart
            show (read s :: Chart) `shouldBe` s

    describe "DataFrame" $ do
        it "addColumn preserves order" $ do
            let f =
                    addColumn "z" (ColNum [3]) $
                        addColumn "y" (ColNum [2]) $
                            addColumn "x" (ColNum [1]) emptyFrame
            columnNames f `shouldBe` ["x", "y", "z"]

        it "addColumn replaces existing in place" $ do
            let f0 =
                    addColumn "y" (ColNum [10]) $
                        addColumn "x" (ColNum [1]) emptyFrame
                f1 = addColumn "x" (ColNum [99]) f0
            columnNames f1 `shouldBe` ["x", "y"]
            lookupColumn "x" f1 `shouldBe` Just (ColNum [99])

        it "frameLength is the longest column" $ do
            let f =
                    addColumn "short" (ColNum [1]) $
                        addColumn "long" (ColNum [1, 2, 3]) emptyFrame
            frameLength f `shouldBe` 3

    describe "Granite.Format" $ do
        it "FormatPrecision 2 uses fixed notation" $
            runFormatter (FormatPrecision 2) 3.14159 `shouldBe` "3.14"
        it "FormatPercent 1 multiplies by 100" $
            runFormatter (FormatPercent 1) 0.345 `shouldBe` "34.5%"
        it "FormatComma groups thousands" $
            runFormatter FormatComma 1234567 `shouldBe` "1,234,567"
        it "FormatSI uses k for thousands" $
            Text.isInfixOf "k" (runFormatter FormatSI 1500) `shouldBe` True
        it "FormatSI uses M for millions" $
            Text.isInfixOf "M" (runFormatter FormatSI 2_500_000) `shouldBe` True
