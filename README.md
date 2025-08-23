# Granite
A library for producing terminal plots. It depends only on Haskell's base and text (only for efficieny but it could use `String`) packages so it should be easy to use and install.

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
![Scatter Plot](https://github.com/mchav/granite/blob/main/static/scatter_plot.png)

```haskell
import Control.Monad
import System.Random.Stateful

import Granite

main :: IO ()
main = do
  g <- newIOGenM =<< newStdGen
  let range = (0 :: Double, 1 :: Double)
  ptsA_x <- replicateM 600 (uniformRM range g)
  ptsA_y <- replicateM 600 (uniformRM range g)
  ptsB_x <- replicateM 600 (uniformRM range g)
  ptsB_y <- replicateM 600 (uniformRM range g)
  putStrLn (scatter "Random points" [series "A" (zip ptsA_x ptsA_y), series "B" (zip ptsB_x ptsB_y)] defPlot{widthChars=68,heightChars=22})
```

### Bar chart
![Bar chart](https://github.com/mchav/granite/blob/main/static/bar_chart.png)

```haskell
import Granite

main :: IO ()
main = putStrLn (bars "Sales" [("Q1",12),("Q2",18),("Q3",9),("Q4",15)] defPlot)
```

### Stacked bar chart
![Stacked bar chart](https://github.com/mchav/granite/blob/main/static/stacked_bar.png)

```haskell
import Granite

main :: IO ()
main =  putStrLn (stackedBars "Quarterly Revenue Breakdown" [ ("Q1", [("Hardware", 120), ("Software", 200), ("Services", 80)])
                                                            , ("Q2", [("Hardware", 135), ("Software", 220), ("Services", 95)])
                                                            , ("Q3", [("Hardware", 110), ("Software", 240), ("Services", 110)])
                                                            , ("Q4", [("Hardware", 145), ("Software", 260), ("Services", 125)])
                                                            ] defPlot)
```

### Pie chart
![Pie chart](https://github.com/mchav/granite/blob/main/static/pie_chart.png)

```haskell
import Granite

main :: IO ()
main = putStrLn (pie "Share" [("Alpha",0.35),("Beta",0.25),("Gamma",0.20),("Delta",0.20)]
                         defPlot{widthChars=46,heightChars=18,legendPos=LegendRight})
```

### Box plot
![Box plot](https://github.com/mchav/granite/blob/main/static/box_plot.png)

```haskell
import Granite

main :: IO ()
main = putStrLn $ boxPlot "Test Score Distribution by Class" [ ("Class A", [78, 82, 85, 88, 90, 92, 85, 87, 89, 91, 76, 94, 88])
                                                             , ("Class B", [70, 75, 72, 80, 85, 78, 82, 77, 79, 81, 74, 83])
                                                             , ("Class C", [88, 92, 95, 90, 93, 89, 91, 94, 96, 87, 90, 92])
                                                             , ("Class D", [65, 70, 72, 68, 75, 80, 73, 71, 69, 74, 77, 76])
                                                             ] defPlot
```

### Line graph
![Line graph](https://github.com/mchav/granite/blob/main/static/line_graph.png)

```haskell
import Granite

main :: IO ()
main = putStrLn $ lineGraph "Monthly Sales Trends" [ ("Product A", [(1, 100), (2, 120), (3, 115), (4, 140), (5, 155), (6, 148)])
                                                   , ("Product B", [(1, 80), (2, 85), (3, 95), (4, 92), (5, 110), (6, 125)])
                                                   , ("Product C", [(1, 60), (2, 62), (3, 70), (4, 85), (5, 82), (6, 90)])
                                                   ] defPlot
```

### Heatmap
![Heatmap](https://github.com/mchav/granite/blob/main/static/heatmap.png)

```haskell
import Granite

main :: IO ()
main = do
  let matrix = [ [1.0,  0.8,  0.3, -0.2,  0.1]
               , [0.8,  1.0,  0.5, -0.1,  0.2]
               , [0.3,  0.5,  1.0,  0.6,  0.4]
               , [-0.2, -0.1, 0.6,  1.0,  0.7]
               , [0.1,  0.2,  0.4,  0.7,  1.0]
               ]
  putStrLn $ heatmap "Correlation Matrix" matrix defPlot
```
