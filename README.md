# Granite
A library for producing terminal plots. It depends only on Haskell's base and text (only for efficiency but it could use `String`. In fact, we expose an API that uses `String` for easier use in GHCi) packages so it should be easy to use and install.

# Supported graph types

* Scatter plots
* Histograms
* (Stacked) bar charts
* Pie charts
* Box plots
* Line charts
* Heat maps


## Examples

### Scatter plot
![Scatter Plot](https://github.com/mchav/granite/blob/main/static/scatter_plot.png?raw=true)

```haskell
import Control.Monad
import System.Random.Stateful

import Granite.String

main :: IO ()
main = do
  g <- newIOGenM =<< newStdGen
  let range = (0 :: Double, 1 :: Double)
  ptsA_x <- replicateM 600 (uniformRM range g)
  ptsA_y <- replicateM 600 (uniformRM range g)
  ptsB_x <- replicateM 600 (uniformRM range g)
  ptsB_y <- replicateM 600 (uniformRM range g)
  putStrLn (scatter [series "A" (zip ptsA_x ptsA_y), series "B" (zip ptsB_x ptsB_y)]
            defPlot{widthChars=68,heightChars=22,plotTitle="Random points"})
```

### Bar chart
![Bar chart](https://github.com/mchav/granite/blob/main/static/bar_chart.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main = T.putStrLn (bars [("Q1",12),("Q2",18),("Q3",9),("Q4",15)] defPlot {plotTitle="Sales"})
```

### Stacked bar chart
![Stacked bar chart](https://github.com/mchav/granite/blob/main/static/stacked_bar.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main =  T.putStrLn (stackedBars [ ("Q1", [("Hardware", 120), ("Software", 200), ("Services", 80)])
                                , ("Q2", [("Hardware", 135), ("Software", 220), ("Services", 95)])
                                , ("Q3", [("Hardware", 110), ("Software", 240), ("Services", 110)])
                                , ("Q4", [("Hardware", 145), ("Software", 260), ("Services", 125)])
                                ] defPlot {plotTitle="Quarterly Revenue Breakdown"})
```

### Pie chart
![Pie chart](https://github.com/mchav/granite/blob/main/static/pie_chart.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main = T.putStrLn (pie [("Alpha",0.35),("Beta",0.25),("Gamma",0.20),("Delta",0.20)] defPlot{widthChars=46,heightChars=18,legendPos=LegendRight,plotTitle="Share"})
```

### Box plot
![Box plot](https://github.com/mchav/granite/blob/main/static/box_plot.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main = T.putStrLn $ boxPlot [ ("Class A", [78, 82, 85, 88, 90, 92, 85, 87, 89, 91, 76, 94, 88])
                            , ("Class B", [70, 75, 72, 80, 85, 78, 82, 77, 79, 81, 74, 83])
                            , ("Class C", [88, 92, 95, 90, 93, 89, 91, 94, 96, 87, 90, 92])
                            , ("Class D", [65, 70, 72, 68, 75, 80, 73, 71, 69, 74, 77, 76])
                            ] defPlot {plotTitle="Test Score Distribution by Class"}
```

### Line graph
![Line graph](https://github.com/mchav/granite/blob/main/static/line_graph.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main = T.putStrLn $ lineGraph [ ("Product A", [(1, 100), (2, 120), (3, 115), (4, 140), (5, 155), (6, 148)])
                              , ("Product B", [(1, 80), (2, 85), (3, 95), (4, 92), (5, 110), (6, 125)])
                              , ("Product C", [(1, 60), (2, 62), (3, 70), (4, 85), (5, 82), (6, 90)])
                              ] defPlot {plotTitle="Monthly Sales Trends"}
```

### Heatmap
![Heatmap](https://github.com/mchav/granite/blob/main/static/heatmap.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T

import Granite

main :: IO ()
main = do
  let matrix = [ [1.0,  0.8,  0.3, -0.2,  0.1]
               , [0.8,  1.0,  0.5, -0.1,  0.2]
               , [0.3,  0.5,  1.0,  0.6,  0.4]
               , [-0.2, -0.1, 0.6,  1.0,  0.7]
               , [0.1,  0.2,  0.4,  0.7,  1.0]
               ]
  T.putStrLn $ heatmap matrix defPlot {plotTitle="Correlation Matrix"}
```

### Histogram
![Histogram](https://github.com/mchav/granite/blob/main/static/histogram.png?raw=true)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Granite
import System.Random.Stateful

main :: IO ()
main = do
  g <- newIOGenM =<< newStdGen
  heights <- replicateM 5000 (uniformRM (160 :: Double, 190 :: Double) g)
  T.putStrLn $
      histogram
          (bins 30 155 195)
          heights
          defPlot
              { widthChars = 68
              , heightChars = 18
              , legendPos = LegendBottom
              , xFormatter = \_ _ v -> T.pack (show (round v :: Int))
              , xNumTicks = 10
              , yNumTicks = 5
              , plotTitle = "Heights (cm)"
              }
```
