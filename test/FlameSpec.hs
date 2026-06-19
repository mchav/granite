{-# LANGUAGE OverloadedStrings #-}

module FlameSpec (spec) where

import Data.Text qualified as Text

import Granite.Flame
import Test.Hspec

-- A 3-level tree: a root with two children, one of which has a child. Mixes
-- increases (fnPos) and decreases (fnNeg) so both colours must appear.
sampleTree :: FlameNode
sampleTree =
    FlameNode
        { fnLabel = "root"
        , fnPos = 9.0e6
        , fnNeg = 7.0e6
        , fnChildren =
            [ FlameNode
                { fnLabel = "Parse.parseModule"
                , fnPos = 8.0e6
                , fnNeg = 2.0e6
                , fnChildren =
                    [ FlameNode "lexer" 1.0e6 3.0e6 []
                    ]
                }
            , FlameNode "Typecheck.check" 1.0e6 5.0e6 []
            ]
        }

spec :: Spec
spec = describe "Granite.Flame.flameDiff" $ do
    let svg = flameDiff sampleTree defFlameOpts

    it "emits a standalone <svg> document" $ do
        svg `shouldSatisfy` Text.isInfixOf "<svg"
        svg `shouldSatisfy` Text.isInfixOf "viewBox"
        svg `shouldSatisfy` Text.isInfixOf "</svg>"

    it "draws both red (increase) and blue (decrease) rects" $ do
        svg `shouldSatisfy` Text.isInfixOf "#ce50"
        svg `shouldSatisfy` Text.isInfixOf "#5064ce"

    it "carries a <title> tooltip with the full label and pos/neg detail" $ do
        svg `shouldSatisfy` Text.isInfixOf "<title>"
        svg `shouldSatisfy` Text.isInfixOf "Parse.parseModule"
        svg `shouldSatisfy` Text.isInfixOf "MB"

    it "renders the title heading" $ do
        flameDiff sampleTree defFlameOpts{foTitle = "My flame"}
            `shouldSatisfy` Text.isInfixOf "My flame"
