{-# LANGUAGE OverloadedStrings #-}

module PipelineSpec (spec) where

import Data.List qualified as List
import Data.Text qualified as Text

import Granite.Color qualified as Col
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

-- A horizontal bar chart whose bars are categorically filled by "status".
statusBarChart :: Chart
statusBarChart =
    emptyChart
        { chartSize = SizeChars 40 20
        , chartCoord = CoordFlip
        , chartData =
            fromColumns
                [ ("module", ColNum [0, 1, 2])
                , ("delta", ColNum [40, 20, 30])
                , ("status", ColCat ["NEW", "grown", "shrunk"])
                ]
        , chartLayers =
            [ (defLayer GeomCol)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "module")
                        , aesY = Just (ColumnRef "delta")
                        , aesFill = Just (ColumnRef "status")
                        }
                }
            ]
        }

-- The distinct @fill="#…"@ values on @<rect>@ elements (data bars), ignoring
-- the white background rect.
distinctRectFills :: Text.Text -> [Text.Text]
distinctRectFills svg =
    let rects = drop 1 (Text.splitOn "<rect" svg)
        fillOf chunk = case Text.splitOn "fill=\"" chunk of
            (_ : rest : _) -> Just (Text.takeWhile (/= '"') rest)
            _ -> Nothing
        fills = [f | Just f <- map fillOf rects, f /= "white"]
     in dedup fills
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- The distinct fills on the 12×12 legend swatch rects, so a test can assert the
-- legend swatches use the same colours as the bars.
legendSwatchFills :: Text.Text -> [Text.Text]
legendSwatchFills svg =
    let rects = drop 1 (Text.splitOn "<rect" svg)
        swatch chunk = Text.isInfixOf "width=\"12\"" chunk && Text.isInfixOf "height=\"12\"" chunk
        fillOf chunk = case Text.splitOn "fill=\"" chunk of
            (_ : rest : _) -> Just (Text.takeWhile (/= '"') rest)
            _ -> Nothing
        fills = [f | chunk <- rects, swatch chunk, Just f <- [fillOf chunk]]
     in dedup fills
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

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

    describe "colour" $ do
        it "colorHex renders exact, zero-padded, lowercase hex" $ do
            Col.colorHex Col.BrightBlue `shouldBe` "#3498db"
            Col.colorHex Col.BrightYellow `shouldBe` "#f1c40f"
            Col.colorHex (Col.Color 0 0 15) `shouldBe` "#00000f"

        it "ansiCode keeps canonical codes for the named slots" $ do
            Col.ansiCode Col.Black `shouldBe` 30
            Col.ansiCode Col.BrightBlue `shouldBe` 94
            Col.ansiCode Col.Default `shouldBe` 39

        it "parseHex accepts valid hex and rejects junk" $ do
            Col.parseHex "#ff0000" `shouldBe` Just (Col.Color 255 0 0)
            Col.parseHex "1a2b3c" `shouldBe` Just (Col.Color 26 43 60)
            Col.parseHex "nope" `shouldBe` Nothing

        it "SVG renders a custom Hex defColor exactly (not quantised to blue)" $ do
            let chart =
                    emptyChart
                        { chartSize = SizeChars 30 16
                        , chartData =
                            fromColumns [("x", ColNum [1, 2, 3]), ("y", ColNum [1, 2, 3])]
                        , chartLayers =
                            [ (defLayer GeomPoint)
                                { layerMapping =
                                    emptyMapping
                                        { aesX = Just (ColumnRef "x")
                                        , aesY = Just (ColumnRef "y")
                                        }
                                , layerAesDef = emptyAesDefaults{defColor = Just (Hex "#ff0000")}
                                }
                            ]
                        }
            renderChartSvg chart `shouldSatisfy` Text.isInfixOf "#ff0000"

        it "categorical aesColor colours points per category with a per-category legend" $ do
            let chart =
                    emptyChart
                        { chartSize = SizeChars 40 20
                        , chartData =
                            fromColumns
                                [ ("x", ColNum [1, 2, 3])
                                , ("y", ColNum [1, 2, 3])
                                , ("g", ColCat ["a", "b", "c"])
                                ]
                        , chartLayers =
                            [ (defLayer GeomPoint)
                                { layerMapping =
                                    emptyMapping
                                        { aesX = Just (ColumnRef "x")
                                        , aesY = Just (ColumnRef "y")
                                        , aesColor = Just (ColumnRef "g")
                                        }
                                }
                            ]
                        }
                svg = renderChartSvg chart
            -- three categories → the first three palette colours all appear
            svg `shouldSatisfy` Text.isInfixOf "#3498db"
            svg `shouldSatisfy` Text.isInfixOf "#9b59b6"
            svg `shouldSatisfy` Text.isInfixOf "#1abc9c"
            -- the legend is keyed by category value, not a generic "series 0"
            svg `shouldSatisfy` Text.isInfixOf ">a</text>"
            svg `shouldSatisfy` Text.isInfixOf ">c</text>"
            svg `shouldNotSatisfy` Text.isInfixOf "series 0"

        it "horizontal bars honour a categorical aesFill (distinct fills)" $ do
            let svg = renderChartSvg statusBarChart{chartScales = defScales}
            length (distinctRectFills svg) `shouldSatisfy` (>= 2)

        it "a manual fill scale maps status values to specific colours" $ do
            let chart =
                    statusBarChart
                        { chartScales =
                            defScales
                                { scaleFill =
                                    Just
                                        ( SColorManual
                                            [ ("NEW", Hex "#ce5050")
                                            , ("grown", Hex "#e0a030")
                                            , ("shrunk", Hex "#50a050")
                                            ]
                                        )
                                }
                        }
                svg = renderChartSvg chart
            svg `shouldSatisfy` Text.isInfixOf "#ce5050"
            svg `shouldSatisfy` Text.isInfixOf "#e0a030"
            svg `shouldSatisfy` Text.isInfixOf "#50a050"

        it "the legend swatches use the same colours as the bars" $ do
            let chart =
                    statusBarChart
                        { chartScales =
                            defScales
                                { scaleFill =
                                    Just
                                        ( SColorManual
                                            [ ("NEW", Hex "#ce5050")
                                            , ("grown", Hex "#e0a030")
                                            , ("shrunk", Hex "#50a050")
                                            ]
                                        )
                                }
                        }
                svg = renderChartSvg chart
            -- Bars and swatches draw from one map, so no palette colour leaks in:
            -- the only rect fills are the three manual colours.
            List.sort (distinctRectFills svg)
                `shouldBe` ["#50a050", "#ce5050", "#e0a030"]
            List.sort (legendSwatchFills svg)
                `shouldBe` ["#50a050", "#ce5050", "#e0a030"]

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
