{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Chart
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Convenience builders that construct a 'Chart' from the same kinds of
arguments accepted by the legacy chart functions in "Granite". Use
these when you want the declarative IR while still keeping ergonomic
construction:

@
import Granite.Chart
import qualified Data.Text.IO as T
import Granite.Render.Pipeline

main = do
  let ch = scatterChart
              [("A", [(0,0),(1,1),(2,4)])
              ,("B", [(0,1),(1,3),(2,6)])
              ] (Just "Random points")
  T.putStrLn (renderChartTerminal ch)
@
-}
module Granite.Chart (
    scatterChart,
    lineChart,
) where

import Data.Text (Text)

import Granite.Spec

{- | Build an IR scatter chart from N series, each with its own (x, y)
pairs. Each series becomes a separate 'GeomPoint' layer carrying its
own data frame and a group aesthetic that drives the legend.
-}
scatterChart :: [(Text, [(Double, Double)])] -> Maybe Text -> Chart
scatterChart sers title =
    emptyChart
        { chartTitle = title
        , chartLayers = map mkLayer sers
        }
  where
    mkLayer (name, pts) =
        let xs = map fst pts
            ys = map snd pts
            df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
         in (defLayer GeomPoint)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesGroup = Just (ColumnRef name)
                        }
                }

{- | Build an IR line chart from N series; analogous to 'scatterChart'
but with one 'GeomLine' layer per series.
-}
lineChart :: [(Text, [(Double, Double)])] -> Maybe Text -> Chart
lineChart sers title =
    emptyChart
        { chartTitle = title
        , chartLayers = map mkLayer sers
        }
  where
    mkLayer (name, pts) =
        let xs = map fst pts
            ys = map snd pts
            df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
         in (defLayer GeomLine)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesGroup = Just (ColumnRef name)
                        }
                }
