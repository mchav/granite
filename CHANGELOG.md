# Revision history for granite

## 0.3.0.3 -- 2025-09-26
* slot width for bar charts now depends on xNumTicks not categories.

## 0.3.0.2 -- 2025-09-26
* Make legend optional.
* xNumTicks now applies to categories.

## 0.3.0.1 -- 2025-09-17
* Left edge of plots now has an elbow instead of disjoint bars.
* Remove dependency on random.

## 0.3.0.0 -- 2025-08-31
* Export a `LabelFormatter` function along with AxisEnv to define smarter labels.
* Change API of formatter to use AxisEnv and slot budget.
* Users can now specify the number of ticks to show and the colour palette for a plot.

## 0.2.0.2 -- 2025-08-30
* Add plot option to define the format of labels on both axes.

## 0.2.0.1 -- 2025-08-26
* Loosen bounds for text

## 0.2.0.0 -- 2025-08-26
* Plot title is now part of the configuration options.
* API now uses Text instead of String.
* You can now specify the bounds of a plot e.g. `T.putStrLn $ lineGraph [("Foo", [(0, 0), (10, 1)]), ("Bar", [(0, 1), (10, 0)])] defPlot { xBounds = (Just 0, Just 10), yBounds = (Just 0, Just 1) }`

## 0.1.0.3 -- 2025-08-21

* Fix heatmaps: matrix grid wasn't properly mapping to canvas + axes were reversed.

## 0.1.0.2 -- 2025-08-20

* Add README to doc files.

## 0.1.0.1 -- 2025-08-20

* Remove IO monad from plotting functions.
* Faster plotting after moving from lists as arrays to AVL trees.
* All functions are now strict by default.

## 0.1.0.0 -- 2025-08-18

* Support for pie charts, bar charts, heatmaps, line graphs, box plots, and histograms.
