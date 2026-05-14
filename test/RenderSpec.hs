{-# LANGUAGE OverloadedStrings #-}

module RenderSpec (spec) where

import Data.Text qualified as Text
import Granite.Color (Color (..))
import Granite.Render.Scene
import Granite.Render.Svg qualified as Svg
import Granite.Render.Terminal qualified as Terminal
import Test.Hspec

-- A small fixture exercising the four marks both backends understand today.
sampleScene :: Scene
sampleScene =
    Scene
        { sceneWidth = 200
        , sceneHeight = 100
        , sceneMarks =
            [ MRect (Rect 10 10 40 30) defaultStyle{styleFill = Just BrightBlue}
            , MCircle (Point 120 50) 12 defaultStyle{styleFill = Just BrightRed}
            , MPolyline
                [Point 20 80, Point 60 70, Point 100 75, Point 160 60]
                defaultStyle{styleStroke = Just BrightGreen, styleStrokeWidth = 2}
            , MText (Point 100 20) "Hello" defaultTextStyle{textFill = BrightYellow}
            ]
        }

spec :: Spec
spec = do
    describe "Granite.Render.Scene + backends" $ do
        it "both backends accept the same Scene" $ do
            let terminal = Terminal.renderScene sampleScene
                svg = Svg.renderScene sampleScene
            Text.length terminal `shouldSatisfy` (> 0)
            Text.length svg `shouldSatisfy` (> 0)

        it "SVG output is a well-formed document with all four marks" $ do
            let svg = Svg.renderScene sampleScene
            svg `shouldSatisfy` Text.isInfixOf "<svg"
            svg `shouldSatisfy` Text.isInfixOf "</svg>"
            svg `shouldSatisfy` Text.isInfixOf "<rect"
            svg `shouldSatisfy` Text.isInfixOf "<circle"
            svg `shouldSatisfy` Text.isInfixOf "<polyline"
            svg `shouldSatisfy` Text.isInfixOf "<text"
            svg `shouldSatisfy` Text.isInfixOf "Hello"

        it "SVG honours the requested viewport" $ do
            let svg = Svg.renderScene sampleScene
            svg `shouldSatisfy` Text.isInfixOf "viewBox=\"0 0 200 100\""

        it "Terminal output contains text marks and is multi-line" $ do
            let terminal = Terminal.renderScene sampleScene
            terminal `shouldSatisfy` Text.isInfixOf "Hello"
            length (Text.lines terminal) `shouldSatisfy` (> 1)

        it "Terminal output dimensions track the scene size" $ do
            let small =
                    Terminal.renderScene
                        sampleScene{sceneWidth = 80, sceneHeight = 32}
                big =
                    Terminal.renderScene
                        sampleScene{sceneWidth = 320, sceneHeight = 128}
            length (Text.lines big) `shouldSatisfy` (> length (Text.lines small))

        it "An empty scene still renders without crashing" $ do
            let empty = Scene 100 50 []
                term = Terminal.renderScene empty
                svg = Svg.renderScene empty
            Text.length term `shouldSatisfy` (>= 0)
            svg `shouldSatisfy` Text.isInfixOf "<svg"

        it "Grouping marks renders the same as flattening them" $ do
            let grouped =
                    sampleScene
                        { sceneMarks =
                            [MGroup (sceneMarks sampleScene)]
                        }
                flat = sampleScene
            Svg.renderScene grouped `shouldSatisfy` Text.isInfixOf "<g>"
            Text.length (Terminal.renderScene grouped)
                `shouldBe` Text.length (Terminal.renderScene flat)
