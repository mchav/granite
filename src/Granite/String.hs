{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Granite.String
-- Copyright   : (c) 2024
-- License     : BSD3
-- Maintainer  : your-email@example.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A String-based interface to the Granite plotting library. This module provides
-- the same functionality as "Granite" but uses 'String' instead of 'Text' for
-- easier use in simple scripts and educational contexts.
--
-- = Basic Usage
--
-- Create a simple scatter plot:
--
-- @
-- import Granite.String
-- 
-- main = do
--   let points = [(x, sin x) | x <- [0, 0.1 .. 6.28]]
--       chart = scatter [series "sin(x)" points] defPlot
--   putStrLn chart
-- @
--
-- = Note on Performance
--
-- This module internally converts between 'String' and 'Text'. For performance-critical
-- applications with large datasets, consider using the "Granite" module directly which
-- works with 'Text' natively.
--
-- = Terminal Requirements
--
-- This library requires a terminal that supports:
--
--   * Unicode (specifically Braille patterns U+2800-U+28FF)
--   * ANSI color codes
--   * Monospace font with proper Braille character rendering

module Granite.String
  (
    -- * Plot Configuration
    Plot(..)
  , defPlot
  , LegendPos
  
    -- * Data Preparation
  , series
  , Bins(..)
  , bins
  
    -- * Chart Types
  , scatter
  , lineGraph
  , bars
  , stackedBars
  , histogram
  , pie
  , heatmap
  , boxPlot
  ) where

import qualified Granite as G
import qualified Data.Text as Text

-- | Position of the legend in the plot.
type LegendPos = G.LegendPos

-- | Re-export LegendPos constructors
pattern LegendRight :: LegendPos
pattern LegendRight = G.LegendRight

pattern LegendBottom :: LegendPos
pattern LegendBottom = G.LegendBottom

-- | Plot configuration parameters.
--
-- Controls the appearance and layout of generated charts.
data Plot = Plot
  { widthChars   :: Int
    -- ^ Width of the plot area in terminal characters (default: 60)
  , heightChars  :: Int
    -- ^ Height of the plot area in terminal characters (default: 20)
  , leftMargin   :: Int
    -- ^ Space reserved for y-axis labels (default: 6)
  , bottomMargin :: Int
    -- ^ Space reserved for x-axis labels (default: 2)
  , titleMargin  :: Int
    -- ^ Space above the plot for the title (default: 1)
  , xBounds      :: (Maybe Double, Maybe Double)
    -- ^ Optional manual x-axis bounds (min, max). 
    -- 'Nothing' uses automatic bounds with 5% padding.
  , yBounds      :: (Maybe Double, Maybe Double)
    -- ^ Optional manual y-axis bounds (min, max).
    -- 'Nothing' uses automatic bounds with 5% padding.
  , plotTitle    :: String
    -- ^ Title displayed above the plot (default: empty)
  , legendPos    :: LegendPos
    -- ^ Position of the legend (default: 'LegendRight')
  } deriving (Eq, Show)

-- | Default plot configuration.
--
-- Creates a 60×20 character plot with reasonable defaults:
--
-- @
-- defPlot = Plot
--   { widthChars   = 60
--   , heightChars  = 20
--   , leftMargin   = 6
--   , bottomMargin = 2
--   , titleMargin  = 1
--   , xBounds      = (Nothing, Nothing)
--   , yBounds      = (Nothing, Nothing)
--   , plotTitle    = ""
--   , legendPos    = LegendRight
--   }
-- @
defPlot :: Plot
defPlot = fromGranitePlot G.defPlot

-- | Configuration for histogram bins.
type Bins = G.Bins

-- | Create a bin configuration for histograms.
--
-- @
-- bins 10 0 100  -- 10 bins from 0 to 100
-- bins 20 (-5) 5 -- 20 bins from -5 to 5
-- @
bins :: Int       -- ^ Number of bins (will be clamped to minimum 1)
     -> Double    -- ^ Lower bound
     -> Double    -- ^ Upper bound
     -> Bins
bins = G.bins

-- | Create a named data series for multi-series plots.
--
-- @
-- let s1 = series "Dataset A" [(1,2), (2,4), (3,6)]
--     s2 = series "Dataset B" [(1,3), (2,5), (3,7)]
--     chart = scatter [s1, s2] defPlot
-- @
series :: String                    -- ^ Name of the series (appears in legend)
       -> [(Double, Double)]         -- ^ List of (x, y) data points
       -> (String, [(Double, Double)])
series name points = (name, points)

-- | Create a scatter plot from multiple data series.
--
-- Each series is rendered with a different color and pattern.
-- Points are plotted using Braille characters for sub-character resolution.
--
-- ==== __Example__
--
-- @
-- let points1 = [(x, x^2) | x <- [-3, -2.5 .. 3]]
--     points2 = [(x, 2*x + 1) | x <- [-3, -2.5 .. 3]]
--     chart = scatter [series "y = x²" points1, 
--                      series "y = 2x + 1" points2] defPlot
-- @
scatter :: [(String, [(Double, Double)])]  -- ^ List of named data series
        -> Plot                             -- ^ Plot configuration
        -> String                           -- ^ Rendered chart as String
scatter seriesList plot = 
  Text.unpack $ G.scatter (map (mapFirst Text.pack) seriesList) (toGranitePlot plot)

-- | Create a line graph connecting data points.
--
-- Similar to 'scatter' but connects consecutive points with lines.
-- Points are automatically sorted by x-coordinate before connecting.
--
-- ==== __Example__
--
-- @
-- let sine = [(x, sin x) | x <- [0, 0.1 .. 2*pi]]
--     cosine = [(x, cos x) | x <- [0, 0.1 .. 2*pi]]
--     chart = lineGraph [series "sin" sine, series "cos" cosine] defPlot
-- @
lineGraph :: [(String, [(Double, Double)])]  -- ^ List of named data series
          -> Plot                             -- ^ Plot configuration
          -> String                           -- ^ Rendered chart as String
lineGraph seriesList plot = 
  Text.unpack $ G.lineGraph (map (mapFirst Text.pack) seriesList) (toGranitePlot plot)

-- | Create a bar chart from categorical data.
--
-- Each bar is colored differently and labeled with its category name.
--
-- ==== __Example__
--
-- @
-- let data = [("Apple", 45.2), ("Banana", 38.1), ("Orange", 52.7)]
--     chart = bars data defPlot { plotTitle = "Fruit Sales" }
-- @
bars :: [(String, Double)]  -- ^ List of (category, value) pairs
     -> Plot                 -- ^ Plot configuration
     -> String               -- ^ Rendered chart as String
bars categories plot = 
  Text.unpack $ G.bars (map (mapFirst Text.pack) categories) (toGranitePlot plot)

-- | Create a stacked bar chart.
--
-- Each category can have multiple stacked components.
--
-- ==== __Example__
--
-- @
-- let data = [("Q1", [("Product A", 100), ("Product B", 150)]),
--             ("Q2", [("Product A", 120), ("Product B", 180)])]
--     chart = stackedBars data defPlot
-- @
stackedBars :: [(String, [(String, Double)])]  -- ^ Categories with stacked components
            -> Plot                             -- ^ Plot configuration
            -> String                           -- ^ Rendered chart as String
stackedBars categories plot = 
  Text.unpack $ G.stackedBars 
    (map (\(cat, items) -> (Text.pack cat, map (mapFirst Text.pack) items)) categories)
    (toGranitePlot plot)

-- | Create a histogram from numerical data.
--
-- Data is binned according to the provided 'Bins' configuration.
--
-- ==== __Example__
--
-- @
-- import System.Random
-- 
-- -- Generate random normal-like distribution
-- let values = take 1000 $ randomRs (0, 100) gen
--     chart = histogram (bins 20 0 100) values defPlot
-- @
histogram :: Bins         -- ^ Binning configuration
          -> [Double]     -- ^ Raw data values to bin
          -> Plot         -- ^ Plot configuration
          -> String       -- ^ Rendered chart as String
histogram binConfig values plot = 
  Text.unpack $ G.histogram binConfig values (toGranitePlot plot)

-- | Create a pie chart showing proportions.
--
-- Values are normalized to sum to 100%. Negative values are treated as zero.
--
-- ==== __Example__
--
-- @
-- let data = [("Chrome", 65), ("Firefox", 20), ("Safari", 10), ("Other", 5)]
--     chart = pie data defPlot { plotTitle = "Browser Market Share" }
-- @
pie :: [(String, Double)]  -- ^ List of (category, value) pairs
    -> Plot                 -- ^ Plot configuration
    -> String               -- ^ Rendered chart as String
pie slices plot = 
  Text.unpack $ G.pie (map (mapFirst Text.pack) slices) (toGranitePlot plot)

-- | Create a heatmap visualization of a 2D matrix.
--
-- Values are mapped to a color gradient from blue (low) to red (high).
--
-- ==== __Example__
--
-- @
-- let matrix = [[x * y | x <- [1..10]] | y <- [1..10]]
--     chart = heatmap matrix defPlot { plotTitle = "Multiplication Table" }
-- @
heatmap :: [[Double]]  -- ^ 2D matrix of values (rows × columns)
        -> Plot         -- ^ Plot configuration
        -> String       -- ^ Rendered chart as String
heatmap matrix plot = 
  Text.unpack $ G.heatmap matrix (toGranitePlot plot)

-- | Create a box plot showing statistical distributions.
--
-- Displays quartiles, median, and min/max values for each dataset.
--
-- ==== __Example__
--
-- @
-- let data1 = [1.2, 2.3, 2.1, 3.4, 2.8, 4.1, 3.9]
--     data2 = [5.1, 4.8, 6.2, 5.9, 7.1, 6.5, 5.5]
--     chart = boxPlot [("Group A", data1), ("Group B", data2)] defPlot
-- @
--
-- The box plot displays:
--
--   * Box: First quartile (Q1) to third quartile (Q3)
--   * Line inside box: Median (Q2)
--   * Whiskers: Minimum and maximum values
boxPlot :: [(String, [Double])]  -- ^ Named datasets
        -> Plot                   -- ^ Plot configuration
        -> String                 -- ^ Rendered chart as String
boxPlot datasets plot = 
  Text.unpack $ G.boxPlot (map (mapFirst Text.pack) datasets) (toGranitePlot plot)

-- | Convert our String-based Plot to Granite's Text-based Plot
toGranitePlot :: Plot -> G.Plot
toGranitePlot p = G.Plot
  { G.widthChars   = widthChars p
  , G.heightChars  = heightChars p
  , G.leftMargin   = leftMargin p
  , G.bottomMargin = bottomMargin p
  , G.titleMargin  = titleMargin p
  , G.xBounds      = xBounds p
  , G.yBounds      = yBounds p
  , G.plotTitle    = Text.pack (plotTitle p)
  , G.legendPos    = legendPos p
  }

-- | Convert Granite's Text-based Plot to our String-based Plot
fromGranitePlot :: G.Plot -> Plot
fromGranitePlot p = Plot
  { widthChars   = G.widthChars p
  , heightChars  = G.heightChars p
  , leftMargin   = G.leftMargin p
  , bottomMargin = G.bottomMargin p
  , titleMargin  = G.titleMargin p
  , xBounds      = G.xBounds p
  , yBounds      = G.yBounds p
  , plotTitle    = Text.unpack (G.plotTitle p)
  , legendPos    = G.legendPos p
  }

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, c) = (f a, c)
