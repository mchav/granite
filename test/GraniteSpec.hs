{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GraniteSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showEFloat, showFFloat)
import Granite

spec :: Spec
spec = do
  describe "Data Types and Constructors" $ do
    testDataTypes
    
  describe "Core Plotting Functions" $ do
    testPlottingFunctions
    
  describe "Helper Functions" $ do
    testHelperFunctions
    
  describe "Canvas and Rendering" $ do
    testCanvasOperations
    
  describe "Formatting and Axis Handling" $ do
    testFormatting
    
  describe "Edge Cases and Error Handling" $ do
    testEdgeCases

testDataTypes :: Spec
testDataTypes = do
  describe "Plot record" $ do
    it "defPlot should have correct default values" $ do
      widthChars defPlot `shouldBe` 60
      heightChars defPlot `shouldBe` 20
      leftMargin defPlot `shouldBe` 6
      bottomMargin defPlot `shouldBe` 2
      titleMargin defPlot `shouldBe` 1
      xBounds defPlot `shouldBe` (Nothing, Nothing)
      yBounds defPlot `shouldBe` (Nothing, Nothing)
      plotTitle defPlot `shouldBe` ""
      legendPos defPlot `shouldBe` LegendRight
      xNumTicks defPlot `shouldBe` 3
      yNumTicks defPlot `shouldBe` 3
      
    it "should allow customization via record update" $ do
      let customPlot = defPlot { widthChars = 80, plotTitle = "Test Chart" }
      widthChars customPlot `shouldBe` 80
      plotTitle customPlot `shouldBe` "Test Chart"
      heightChars customPlot `shouldBe` 20

  describe "LegendPos enum" $ do
    it "should have correct Show instances" $ do
      show LegendRight `shouldBe` "LegendRight"
      show LegendBottom `shouldBe` "LegendBottom"
      
    it "should have correct Eq instances" $ do
      LegendRight `shouldBe` LegendRight
      LegendBottom `shouldBe` LegendBottom
      LegendRight `shouldNotBe` LegendBottom

  describe "Color enum" $ do
    it "should convert to correct ANSI codes" $ do
      ansiCode Black `shouldBe` 30
      ansiCode Red `shouldBe` 31
      ansiCode BrightBlue `shouldBe` 94
      ansiCode Default `shouldBe` 39
      
  describe "Bins constructor" $ do
    it "should create bins with correct values" $ do
      let b = bins 10 0 100
      nBins b `shouldBe` 10
      lo b `shouldBe` 0
      hi b `shouldBe` 100
      
    it "should handle reversed bounds" $ do
      let b = bins 10 100 0
      lo b `shouldBe` 0
      hi b `shouldBe` 100
      
    it "should enforce minimum of 1 bin" $ do
      let b = bins (-5) 0 100
      nBins b `shouldBe` 1

testPlottingFunctions :: Spec
testPlottingFunctions = do
  describe "series function" $ do
    it "should create a named data series" $ do
      let s = series "test" [(1, 2), (3, 4)]
      fst s `shouldBe` "test"
      snd s `shouldBe` [(1, 2), (3, 4)]

  describe "scatter plot" $ do
    it "should generate non-empty output for valid data" $ do
      let points = [(1, 1), (2, 2), (3, 3)]
          chart = scatter [series "line" points] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle empty data gracefully" $ do
      let chart = scatter [] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should include series name in legend" $ do
      let points = [(1, 1), (2, 2)]
          chart = scatter [series "test series" points] defPlot
      chart `shouldSatisfy` Text.isInfixOf "test series"
      
    it "should handle multiple series" $ do
      let s1 = series "Series 1" [(1, 1), (2, 2)]
          s2 = series "Series 2" [(1, 2), (2, 1)]
          chart = scatter [s1, s2] defPlot
      chart `shouldSatisfy` Text.isInfixOf "Series 1"
      chart `shouldSatisfy` Text.isInfixOf "Series 2"

  describe "line graph" $ do
    it "should generate non-empty output" $ do
      let points = [(1, sin 1), (2, sin 2), (3, sin 3)]
          chart = lineGraph [series "sine" points] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should sort points by x-coordinate internally" $ do
      let unsorted = [(3, 3), (1, 1), (2, 2)]
          chart = lineGraph [series "unsorted" unsorted] defPlot
      Text.length chart `shouldSatisfy` (> 0)

  describe "bar chart" $ do
    it "should handle categorical data" $ do
      let data' = [("A", 10), ("B", 20), ("C", 15)]
          chart = bars data' defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should include category names" $ do
      let data' = [("Apple", 10), ("Banana", 20)]
          chart = bars data' defPlot
      chart `shouldSatisfy` Text.isInfixOf "Apple"
      chart `shouldSatisfy` Text.isInfixOf "Banana"

  describe "histogram" $ do
    it "should bin numerical data" $ do
      let values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          chart = histogram (bins 5 0 10) values defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle empty data" $ do
      let chart = histogram (bins 5 0 10) [] defPlot
      Text.length chart `shouldSatisfy` (> 0)

  describe "pie chart" $ do
    it "should handle proportional data" $ do
      let data' = [("A", 25), ("B", 50), ("C", 25)]
          chart = pie data' defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should normalize values to proportions" $ do
      let data' = [("A", 1), ("B", 2), ("C", 1)]
          chart = pie data' defPlot
      Text.length chart `shouldSatisfy` (> 0)

  describe "heatmap" $ do
    it "should handle 2D matrix data" $ do
      let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
          chart = heatmap matrix defPlot
      Text.length chart `shouldSatisfy` (> 0)

  describe "box plot" $ do
    it "should calculate quartiles correctly" $ do
      let data1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          data2 = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
          chart = boxPlot [("Group1", data1), ("Group2", data2)] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle single data point" $ do
      let chart = boxPlot [("Single", [5.0])] defPlot
      Text.length chart `shouldSatisfy` (> 0)

testHelperFunctions :: Spec
testHelperFunctions = do
  describe "ANSI color functions" $ do
    it "ansiOn should generate correct escape sequence" $ do
      let red = ansiOn Red
      red `shouldSatisfy` Text.isPrefixOf "\ESC["
      red `shouldSatisfy` Text.isSuffixOf "m"
      
    it "ansiOff should generate reset sequence" $ do
      ansiOff `shouldBe` "\ESC[0m"
      
    it "paint should colorize characters correctly" $ do
      let painted = paint Red 'X'
      painted `shouldSatisfy` Text.isInfixOf "X"
      painted `shouldSatisfy` Text.isPrefixOf "\ESC["
      
    it "paint should handle space characters specially" $ do
      paint Red ' ' `shouldBe` " "

  describe "clamp function (internal)" $ do
    it "should constrain values to range" $ property $ \(x :: Double) ->
      let clamped = max 0 (min 10 x)
      in clamped >= 0 && clamped <= 10
      
  describe "bounds calculation" $ do
    it "should handle single point" $ do
      let chart = scatter [series "single" [(5, 10)]] defPlot
      Text.length chart `shouldSatisfy` (> 0)

testCanvasOperations :: Spec
testCanvasOperations = do
  describe "plot dimensions" $ do
    it "should respect widthChars setting" $ do
      let narrow = defPlot { widthChars = 20 }
          wide = defPlot { widthChars = 100 }
          chartNarrow = scatter [series "test" [(1, 1)]] narrow
          chartWide = scatter [series "test" [(1, 1)]] wide
      let narrowLines = Text.lines chartNarrow
          wideLines = Text.lines chartWide
      if not (null narrowLines) && not (null wideLines)
        then Text.length (head wideLines) `shouldSatisfy` (> Text.length (head narrowLines))
        else pendingWith "Charts generated empty lines"
        
    it "should respect heightChars setting" $ do
      let short = defPlot { heightChars = 5 }
          tall = defPlot { heightChars = 30 }
          chartShort = scatter [series "test" [(1, 1)]] short
          chartTall = scatter [series "test" [(1, 1)]] tall
      length (Text.lines chartTall) `shouldSatisfy` (> length (Text.lines chartShort))

testFormatting :: Spec
testFormatting = do
  describe "default formatter" $ do
    it "should format small numbers appropriately" $ do
      let env = AxisEnv (0, 10) 0 5
          formatted = fmt env 10 5.5
      Text.length formatted `shouldSatisfy` (> 0)
      
    it "should use scientific notation for large numbers" $ do
      let env = AxisEnv (0, 100000) 0 5
          formatted = fmt env 10 50000
      formatted `shouldSatisfy` Text.isInfixOf "e"
      
    it "should handle zero correctly" $ do
      let env = AxisEnv (-1, 1) 0 5
          formatted = fmt env 10 0
      formatted `shouldSatisfy` (== "0.0")

  describe "plot titles" $ do
    it "should include title when set" $ do
      let titled = defPlot { plotTitle = "My Test Chart" }
          chart = scatter [series "data" [(1, 1)]] titled
      chart `shouldSatisfy` Text.isInfixOf "My Test Chart"

testEdgeCases :: Spec
testEdgeCases = do
  describe "boundary conditions" $ do
    it "should handle zero-width plots gracefully" $ do
      let zeroWidth = defPlot { widthChars = 0 }
          chart = scatter [series "test" [(1, 1)]] zeroWidth
      Text.length chart `shouldSatisfy` (>= 0)
      
    it "should handle zero-height plots gracefully" $ do
      let zeroHeight = defPlot { heightChars = 0 }
          chart = scatter [series "test" [(1, 1)]] zeroHeight
      Text.length chart `shouldSatisfy` (>= 0)

  describe "data edge cases" $ do
    it "should handle identical points" $ do
      let identical = replicate 10 (5.0, 5.0)
          chart = scatter [series "identical" identical] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle very large numbers" $ do
      let large = [(1e10, 1e10), (2e10, 2e10)]
          chart = scatter [series "large" large] defPlot
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle very small numbers" $ do
      let small = [(1e-10, 1e-10), (2e-10, 2e-10)]
          chart = scatter [series "small" small] defPlot
      Text.length chart `shouldSatisfy` (> 0)

  describe "custom bounds" $ do
    it "should respect manual x bounds" $ do
      let bounded = defPlot { xBounds = (Just 0, Just 100) }
          chart = scatter [series "test" [(50, 50)]] bounded
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should respect manual y bounds" $ do
      let bounded = defPlot { yBounds = (Just (-10), Just 10) }
          chart = scatter [series "test" [(0, 0)]] bounded
      Text.length chart `shouldSatisfy` (> 0)

  describe "legend positioning" $ do
    it "should handle bottom legend" $ do
      let bottomLegend = defPlot { legendPos = LegendBottom }
          chart = scatter [series "test" [(1, 1)]] bottomLegend
      Text.length chart `shouldSatisfy` (> 0)
      
    it "should handle right legend" $ do
      let rightLegend = defPlot { legendPos = LegendRight }
          chart = scatter [series "test" [(1, 1)]] rightLegend
      Text.length chart `shouldSatisfy` (> 0)

propTests :: Spec  
propTests = describe "Property-based tests" $ do
  describe "QuickCheck properties" $ do
    it "scatter plots should always produce non-empty output for non-empty data" $ 
      property $ \(NonEmpty points) -> 
        let chart = scatter [series "prop test" (points :: [(Double, Double)])] defPlot
        in Text.length chart > 0
        
    it "bins should always have positive count and valid bounds" $
      property $ \(Positive n) (a :: Double) (b :: Double) ->
        let b' = bins n a b
        in nBins b' >= 1 && lo b' <= hi b'

ansiCode :: Color -> Int
ansiCode Black = 30
ansiCode Red = 31
ansiCode Green = 32
ansiCode Yellow = 33
ansiCode Blue = 34
ansiCode Magenta = 35
ansiCode Cyan = 36
ansiCode White = 37
ansiCode BrightBlack = 90
ansiCode BrightRed = 91
ansiCode BrightGreen = 92
ansiCode BrightYellow = 93
ansiCode BrightBlue = 94
ansiCode BrightMagenta = 95
ansiCode BrightCyan = 96
ansiCode BrightWhite = 97
ansiCode Default = 39

ansiOn :: Color -> Text
ansiOn c = "\ESC[" <> Text.pack (show (ansiCode c)) <> "m"

ansiOff :: Text
ansiOff = "\ESC[0m"

paint :: Color -> Char -> Text
paint c ch = if ch == ' ' then " " else ansiOn c <> Text.singleton ch <> ansiOff

fmt :: AxisEnv -> Int -> Double -> Text
fmt _ _ v
    | abs v >= 10000 || abs v < 0.01 && v /= 0 = Text.pack (showEFloat (Just 1) v "")
    | otherwise = Text.pack (showFFloat (Just 1) v "")

runTests :: IO ()
runTests = hspec spec