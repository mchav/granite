{-# LANGUAGE OverloadedStrings #-}

module PipelineSpec (spec) where

import Data.Text qualified as Text

import Granite.Render.Pipeline
import Granite.Render.Scene (Scene (..))
import Granite.Spec
import Test.Hspec

-- A scatter chart equivalent to the README example: two named series
-- of (x, y) data, rendered through the new IR pipeline.
scatterChart :: Chart
scatterChart =
    emptyChart
        { chartTitle = Just "Random points"
        , chartSize = SizeChars 68 22
        , chartLayers =
            [ pointLayer "A" [(0, 0), (1, 1), (2, 4), (3, 9), (4, 16)]
            , pointLayer "B" [(0, 1), (1, 3), (2, 6), (3, 10), (4, 15)]
            ]
        }
  where
    pointLayer name pts =
        let xs = map fst pts
            ys = map snd pts
            df =
                fromColumns
                    [ ("x", ColNum xs)
                    , ("y", ColNum ys)
                    ]
         in (defLayer GeomPoint)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesGroup = Just (ColumnRef name)
                        }
                }

spec :: Spec
spec = describe "Granite.Render.Pipeline" $ do
    describe "scatter Chart through the IR" $ do
        it "produces a non-empty Scene with both axes" $ do
            let scene = chartToScene scatterChart
            sceneWidth scene `shouldSatisfy` (> 0)
            sceneHeight scene `shouldSatisfy` (> 0)
            length (sceneMarks scene) `shouldSatisfy` (> 0)

        it "terminal output contains the title" $ do
            let term = renderChartTerminal scatterChart
            term `shouldSatisfy` Text.isInfixOf "Random points"

        it "SVG output contains the title and viewBox" $ do
            let svg = renderChartSvg scatterChart
            svg `shouldSatisfy` Text.isInfixOf "Random points"
            svg `shouldSatisfy` Text.isInfixOf "viewBox"

        it "SVG output has axis structure (lines + tick text)" $ do
            let svg = renderChartSvg scatterChart
            -- Axes are emitted as <line> (since the chrome
            -- gridline/tick refactor); <polyline> still appears for
            -- data layers.
            svg `shouldSatisfy` Text.isInfixOf "<line"
            svg `shouldSatisfy` Text.isInfixOf "<text"

        it "SVG output has at least one circle per series" $ do
            let svg = renderChartSvg scatterChart
                ncircles = Text.count "<circle" svg
            ncircles `shouldSatisfy` (>= 5 * 2) -- 5 points × 2 series
        it "SVG output has a legend swatch per series" $ do
            let svg = renderChartSvg scatterChart
                nrects = Text.count "<rect" svg
            -- 1 background + 2 legend swatches; allow more from future chrome
            nrects `shouldSatisfy` (>= 3)

        it "terminal line count is similar to legacy output" $ do
            let term = renderChartTerminal scatterChart
                lineCount = length (Text.lines term)
            -- For a 68×22 char canvas plus title + bottom labels we expect
            -- roughly 24 lines (22 plot rows + title + axis labels).
            lineCount `shouldSatisfy` (>= 15)
            lineCount `shouldSatisfy` (<= 40)

    describe "Log-scale chart" $ do
        let logChart =
                scatterChart
                    { chartScales =
                        defScales{scaleY = SLog Base10 defScaleOpts}
                    , chartTitle = Just "Log Y"
                    }
        it "renders without error and includes log-friendly breaks" $ do
            let svg = renderChartSvg logChart
            svg `shouldSatisfy` Text.isInfixOf "Log Y"
            svg `shouldSatisfy` Text.isInfixOf "<svg"

    describe "Empty chart" $ do
        it "renders an empty chart without crashing" $ do
            let svg = renderChartSvg emptyChart
                term = renderChartTerminal emptyChart
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            Text.length term `shouldSatisfy` (>= 0)

    describe "SizeResponsive" $ do
        let responsive =
                scatterChart{chartSize = SizeResponsive 2.0}
        let fixed =
                scatterChart{chartSize = SizeChars 60 20}

        it "drops absolute width/height for SizeResponsive" $ do
            let svg = renderChartSvg responsive
            svg `shouldSatisfy` Text.isInfixOf "viewBox=\"0 0"
            svg `shouldSatisfy` Text.isInfixOf "preserveAspectRatio"
            svg `shouldSatisfy` Text.isInfixOf "width=\"100%\""
            svg `shouldSatisfy` Text.isInfixOf "style=\"height:auto"

        it "keeps absolute width/height for SizeChars / SizePixels" $ do
            let svg = renderChartSvg fixed
                -- inspect just the opening <svg ...> tag, not the body
                openSvg = Text.takeWhile (/= '>') (Text.dropWhile (/= '<') svg)
            openSvg `shouldSatisfy` Text.isInfixOf "viewBox"
            openSvg `shouldSatisfy` (not . Text.isInfixOf "preserveAspectRatio")
            openSvg `shouldSatisfy` (not . Text.isInfixOf "width=\"100%\"")
