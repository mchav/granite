{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Internal.LegacyChart
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Chart builders shared between "Granite" and "Granite.Svg" for the
plotly-express-style helpers (area, ribbon, density, errorBars,
funnel, polarLine, waterfall, distPlot).
-}
module Granite.Internal.LegacyChart (
    Chart,
    plotChart,
    areaChart,
    ribbonChart,
    densityChart,
    errorBarsChart,
    funnelChart,
    polarLineChart,
    waterfallChart,
    distPlotChart,
) where

import Data.Text (Text)
import Data.Text qualified as Text

import Granite.Spec

plotChart :: Int -> Int -> Text -> Chart -> Chart
plotChart w h title chart =
    chart
        { chartSize = SizeChars w h
        , chartTitle = if Text.null title then Nothing else Just title
        }

areaChart :: [(Text, [(Double, Double)])] -> Int -> Int -> Text -> Chart
areaChart series w h title =
    let layers = concatMap mkSeries series
     in plotChart w h title $ emptyChart{chartLayers = layers}
  where
    mkSeries (name, pts) =
        let xs = map fst pts
            ys = map snd pts
            zeros = replicate (length pts) 0 :: [Double]
            df =
                fromColumns
                    [ ("x", ColNum xs)
                    , ("y", ColNum ys)
                    , ("ymin", ColNum zeros)
                    ]
            ribbonL =
                (defLayer GeomRibbon)
                    { layerData = Just df
                    , layerMapping =
                        emptyMapping
                            { aesX = Just (ColumnRef "x")
                            , aesYmin = Just (ColumnRef "ymin")
                            , aesYmax = Just (ColumnRef "y")
                            , aesGroup = Just (ColumnRef name)
                            }
                    , layerAesDef = emptyAesDefaults{defAlpha = Just 0.35}
                    }
            lineL =
                (defLayer GeomLine)
                    { layerData = Just df
                    , layerMapping =
                        emptyMapping
                            { aesX = Just (ColumnRef "x")
                            , aesY = Just (ColumnRef "y")
                            , aesGroup = Just (ColumnRef name)
                            }
                    }
         in [ribbonL, lineL]

ribbonChart ::
    [(Text, [(Double, Double, Double)])] ->
    Int ->
    Int ->
    Text ->
    Chart
ribbonChart series w h title =
    let layers = map mkLayer series
     in plotChart w h title $ emptyChart{chartLayers = layers}
  where
    mkLayer (name, pts) =
        let xs = [x | (x, _, _) <- pts]
            los = [l | (_, l, _) <- pts]
            his = [hi | (_, _, hi) <- pts]
            df =
                fromColumns
                    [ ("x", ColNum xs)
                    , ("ymin", ColNum los)
                    , ("ymax", ColNum his)
                    ]
         in (defLayer GeomRibbon)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesYmin = Just (ColumnRef "ymin")
                        , aesYmax = Just (ColumnRef "ymax")
                        , aesGroup = Just (ColumnRef name)
                        }
                , layerAesDef = emptyAesDefaults{defAlpha = Just 0.4}
                }

densityChart :: [(Text, [Double])] -> Int -> Int -> Text -> Chart
densityChart series w h title =
    let layers = map mkLayer series
     in plotChart w h title $ emptyChart{chartLayers = layers}
  where
    mkLayer (name, sample) =
        let df = fromColumns [("x", ColNum sample)]
         in (defLayer GeomDensity)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "density")
                        , aesGroup = Just (ColumnRef name)
                        }
                , layerStat = StatDensity
                }

errorBarsChart ::
    [(Text, [(Double, Double, Double, Double)])] ->
    Int ->
    Int ->
    Text ->
    Chart
errorBarsChart series w h title =
    let layers = concatMap mkSeries series
     in plotChart w h title $ emptyChart{chartLayers = layers}
  where
    mkSeries (name, rows) =
        let xs = [x | (x, _, _, _) <- rows]
            ys = [y | (_, y, _, _) <- rows]
            los = [l | (_, _, l, _) <- rows]
            his = [hi | (_, _, _, hi) <- rows]
            df =
                fromColumns
                    [ ("x", ColNum xs)
                    , ("y", ColNum ys)
                    , ("lo", ColNum los)
                    , ("hi", ColNum his)
                    ]
            m =
                emptyMapping
                    { aesX = Just (ColumnRef "x")
                    , aesY = Just (ColumnRef "y")
                    , aesYmin = Just (ColumnRef "lo")
                    , aesYmax = Just (ColumnRef "hi")
                    , aesGroup = Just (ColumnRef name)
                    }
            bars =
                (defLayer GeomErrorbar)
                    { layerData = Just df
                    , layerMapping = m
                    , layerStat = StatIdentity
                    }
            pts =
                (defLayer GeomPoint)
                    { layerData = Just df
                    , layerMapping = m
                    , layerStat = StatIdentity
                    }
         in [bars, pts]

funnelChart :: [(Text, Double)] -> Int -> Int -> Text -> Chart
funnelChart stages w h title =
    let names = [n | (n, _) <- stages]
        values = [v | (_, v) <- stages]
        df =
            fromColumns
                [ ("stage", ColCat names)
                , ("value", ColNum values)
                ]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "stage")
                        , aesY = Just (ColumnRef "value")
                        }
                , layerStat = StatIdentity
                }
     in plotChart w h title $
            emptyChart
                { chartData = df
                , chartLayers = [layer]
                , chartCoord = CoordFlip
                }

polarLineChart :: [(Text, [(Double, Double)])] -> Int -> Int -> Text -> Chart
polarLineChart series w h title =
    let layers = map mkLayer series
     in plotChart w h title $
            emptyChart
                { chartLayers = layers
                , chartCoord = CoordPolar ThetaX 0 PolarCCW
                }
  where
    mkLayer (name, pts) =
        let thetas = [t | (t, _) <- pts]
            rs = [r | (_, r) <- pts]
            df = fromColumns [("theta", ColNum thetas), ("r", ColNum rs)]
         in (defLayer GeomLine)
                { layerData = Just df
                , layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "theta")
                        , aesY = Just (ColumnRef "r")
                        , aesGroup = Just (ColumnRef name)
                        }
                }

waterfallChart :: [(Text, Double, Double)] -> Int -> Int -> Text -> Chart
waterfallChart rows w h title =
    let labels = [n | (n, _, _) <- rows]
        starts = [s | (_, s, _) <- rows]
        ends = [e | (_, _, e) <- rows]
        df =
            fromColumns
                [ ("x", ColCat labels)
                , ("y", ColNum ends)
                , ("__ybase", ColNum starts)
                ]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerStat = StatIdentity
                }
     in plotChart w h title $
            emptyChart
                { chartData = df
                , chartLayers = [layer]
                }

distPlotChart :: [(Text, [Double])] -> Int -> Int -> Text -> Chart
distPlotChart series w h title =
    let layers = concatMap mkSeries series
     in plotChart w h title $ emptyChart{chartLayers = layers}
  where
    mkSeries (name, sample) =
        let df = fromColumns [("x", ColNum sample)]
            hist =
                (defLayer GeomHistogram)
                    { layerData = Just df
                    , layerMapping =
                        emptyMapping
                            { aesX = Just (ColumnRef "x")
                            , aesY = Just (ColumnRef "count")
                            , aesGroup = Just (ColumnRef name)
                            }
                    , layerStat = StatBin (BinByCount 20)
                    , layerAesDef = emptyAesDefaults{defAlpha = Just 0.6}
                    }
            kde =
                (defLayer GeomDensity)
                    { layerData = Just df
                    , layerMapping =
                        emptyMapping
                            { aesX = Just (ColumnRef "x")
                            , aesY = Just (ColumnRef "density")
                            , aesGroup = Just (ColumnRef name)
                            }
                    , layerStat = StatDensity
                    }
         in [hist, kde]
