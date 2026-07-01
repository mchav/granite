<!-- scripths: 0.5.3.0 -->

# Granite Tutorial

A walk through every chart granite can produce, as runnable code cells.
Each cell prints its chart as SVG; the rendered output is shown beneath it.

> This is the **base** version of the tutorial and the source of truth. It is a
> [scripths](https://github.com/mchav/scripths) notebook: each Haskell cell is
> evaluated in order and its captured output (the SVG) is inlined beneath the
> fence. Edit this file, then regenerate the rendered companion `docs/tutorial.md`:
>
> ```sh
> scripths -o docs/tutorial.md docs/base/tutorial.md
> ```
>
> Run it from inside the granite repo so the cells build against the local working
> tree (scripths auto-detects the enclosing `granite` package). Do not edit
> `docs/tutorial.md` by hand.

## Contents

- [Mental model](#mental-model)
- [Scatter](#scatter)
- [Coloured scatter](#coloured-scatter)
- [Line](#line)
- [Bar (vertical)](#bar-vertical)
- [Horizontal bar](#horizontal-bar)
- [Stacked bar](#stacked-bar)
- [Grouped (dodged) bar](#grouped-dodged-bar)
- [Pie](#pie)
- [Filled area / ribbon](#filled-area--ribbon)
- [Histogram](#histogram)
- [Box plot](#box-plot)
- [Error bars](#error-bars)
- [Density (KDE)](#density-kde)
- [Distplot (histogram + density)](#distplot-histogram--density)
- [Gauss (z-score distribution)](#gauss-z-score-distribution)
- [Heatmap](#heatmap)
- [Annotated heatmap](#annotated-heatmap)
- [Funnel](#funnel)
- [Waterfall](#waterfall)
- [Polar charts](#polar-charts)
- [Faceted charts](#faceted-charts)
- [Log-Y scatter](#log-y-scatter)
- [Layered charts](#layered-charts)
  - [Bar + trend line](#bar--trend-line)
  - [Scatter + radius circle](#scatter--radius-circle)
  - [Scatter + best-fit line](#scatter--best-fit-line)
  - [Line + confidence ribbon](#line--confidence-ribbon)

---

## Mental model

A `Chart` is a Grammar-of-Graphics-style description:

```
Chart = data + layers + scales + coord + facet + theme + size
```

- **data** — a `DataFrame` with named columns
- **layers** — one or more `Layer`s, each pairing a `Geom` with an
  aesthetic mapping
- **scales** — how data maps to screen (linear / log / sqrt / reverse)
- **coord** — coordinate system (`Cartesian`, `Flip`, or `Polar`)
- **facet** — split the data into a grid of panels
- **theme** — colors, font sizes, palette
- **size** — terminal cells, pixels, or responsive aspect

The pipeline turns the spec into a backend-agnostic `Scene` of
primitive marks, then either backend (`renderChartTerminal` or
`renderChartSvg`) emits text or SVG.

---

## Scatter

A scatter plot: one circle per row at `(aesX, aesY)`.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [0, 1, 2, 3, 4] :: [Double]
ys = [0, 1, 4, 9, 16] :: [Double]
df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
layer =
    (defLayer GeomPoint)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "y = x^2"
        , chartSize = SizeChars 40 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 224" width="400" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="280" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="65.23" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="116.36" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="167.50" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="218.64" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">3.0</text>
> <text x="269.77" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="146.41" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="49" y="100.33" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="54.26" text-anchor="end" fill="#7f8c8d" font-size="11">15.0</text>
> <circle cx="65.23" cy="188.83" r="3" fill="#3498db"/>
> <circle cx="116.36" cy="179.61" r="3" fill="#3498db"/>
> <circle cx="167.50" cy="151.96" r="3" fill="#3498db"/>
> <circle cx="218.64" cy="105.88" r="3" fill="#3498db"/>
> <circle cx="269.77" cy="41.37" r="3" fill="#3498db"/>
> <text x="167.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">y = x^2</text>
> <rect x="295" y="39" width="12" height="12" fill="#3498db"/>
> <text x="311" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Coloured scatter

Map a categorical column to `aesColor`: each category is drawn in its own
palette colour, with a matching legend entry. (Categorical colour is honoured
for `GeomPoint`; a numeric colour column keeps a single colour.)

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
ys = [2, 3, 5, 4, 6, 8, 7, 9, 11] :: [Double]
groups =
    [ "control", "control", "control"
    , "treated", "treated", "treated"
    , "boosted", "boosted", "boosted"
    ]
df =
    fromColumns
        [ ("x", ColNum xs)
        , ("y", ColNum ys)
        , ("group", ColCat groups)
        ]
layer =
    (defLayer GeomPoint)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                , aesColor = Just (ColumnRef "group")
                }
        , layerAesDef = emptyAesDefaults { defSize = Just 5 }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Measurements by group"
        , chartSize = SizeChars 44 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 440 256" width="440" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="320" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="97.16" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="157.39" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="217.61" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="277.84" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">8.0</text>
> <text x="49" y="223.04" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="183.81" text-anchor="end" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="144.57" text-anchor="end" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="105.34" text-anchor="end" fill="#7f8c8d" font-size="11">8.0</text>
> <text x="49" y="66.11" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <circle cx="67.05" cy="219.37" r="5" fill="#3498db"/>
> <circle cx="97.16" cy="199.76" r="5" fill="#3498db"/>
> <circle cx="127.27" cy="160.52" r="5" fill="#3498db"/>
> <circle cx="157.39" cy="180.14" r="5" fill="#9b59b6"/>
> <circle cx="187.50" cy="140.91" r="5" fill="#9b59b6"/>
> <circle cx="217.61" cy="101.68" r="5" fill="#9b59b6"/>
> <circle cx="247.73" cy="121.29" r="5" fill="#1abc9c"/>
> <circle cx="277.84" cy="82.06" r="5" fill="#1abc9c"/>
> <circle cx="307.95" cy="42.83" r="5" fill="#1abc9c"/>
> <text x="187.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Measurements by group</text>
> <rect x="335" y="39" width="12" height="12" fill="#3498db"/>
> <text x="351" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">control</text>
> <rect x="335" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="351" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">treated</text>
> <rect x="335" y="73" width="12" height="12" fill="#1abc9c"/>
> <text x="351" y="83" text-anchor="start" fill="#7f8c8d" font-size="11">boosted</text>
> </svg>

---

## Line

A line chart: points are sorted by X, then connected with a polyline.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [0, 0.5 .. 6.0] :: [Double]
ys = map sin xs
df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
layer =
    (defLayer GeomLine)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "sin(x)"
        , chartSize = SizeChars 50 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 224" width="500" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="380" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="69.77" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="168.26" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="266.74" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="365.23" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="194.17" text-anchor="end" fill="#7f8c8d" font-size="11">-1.0</text>
> <text x="49" y="119.51" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="44.85" text-anchor="end" fill="#7f8c8d" font-size="11">1.0</text>
> <polyline points="69.77,115.85 94.39,80.05 119.02,53.02 143.64,41.37 168.26,47.96 192.88,71.16 217.50,105.31 242.12,142.03 266.74,172.35 291.36,188.83 315.98,187.44 340.61,168.52 365.23,136.71" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="217.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">sin(x)</text>
> <rect x="395" y="39" width="12" height="12" fill="#3498db"/>
> <text x="411" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Bar (vertical)

> **Gotcha**: `defLayer GeomBar` defaults `layerStat = StatCount`, which
> overwrites Y with per-X counts. For pre-aggregated bars, override to
> `StatIdentity` (the ggplot `stat="identity"` convention).

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = ["A", "B", "C", "D"]
ys = [12, 7, 18, 9] :: [Double]
df = fromColumns [("x", ColCat xs), ("y", ColNum ys)]
layer =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        , layerStat = StatIdentity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Bars"
        , chartSize = SizeChars 40 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 224" width="400" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="280" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="86.76" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">A</text>
> <text x="140.59" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">B</text>
> <text x="194.41" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">C</text>
> <text x="248.24" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">D</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="151.53" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="49" y="110.57" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="69.62" text-anchor="end" fill="#7f8c8d" font-size="11">15.0</text>
> <rect x="65.23" y="90.52" width="43.06" height="98.30" fill="#3498db"/>
> <rect x="119.06" y="131.48" width="43.06" height="57.34" fill="#3498db"/>
> <rect x="172.88" y="41.37" width="43.06" height="147.45" fill="#3498db"/>
> <rect x="226.71" y="115.10" width="43.06" height="73.73" fill="#3498db"/>
> <text x="167.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Bars</text>
> <rect x="295" y="39" width="12" height="12" fill="#3498db"/>
> <text x="311" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Horizontal bar

The same chart, rendered with `CoordFlip` so X data lands on the
vertical axis.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = ["Alpha", "Bravo", "Charlie", "Delta", "Echo"]
ys = [82, 67, 45, 33, 21] :: [Double]
df = fromColumns [("item", ColCat xs), ("value", ColNum ys)]
layer =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "item")
                , aesY = Just (ColumnRef "value")
                }
        , layerStat = StatIdentity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartCoord = CoordFlip
        , chartTitle = Just "Top 5 (horizontal)"
        , chartSize = SizeChars 60 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 256" width="600" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="58.20" y1="228.20" x2="480.00" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="58.20" y1="34" x2="58.20" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="77.37" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="170.90" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">20.0</text>
> <text x="264.42" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">40.0</text>
> <text x="357.95" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">60.0</text>
> <text x="451.47" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">80.0</text>
> <text x="52.20" y="61.21" text-anchor="end" fill="#7f8c8d" font-size="11">Alpha</text>
> <text x="52.20" y="97.99" text-anchor="end" fill="#7f8c8d" font-size="11">Bravo</text>
> <text x="52.20" y="134.77" text-anchor="end" fill="#7f8c8d" font-size="11">Charlie</text>
> <text x="52.20" y="171.55" text-anchor="end" fill="#7f8c8d" font-size="11">Delta</text>
> <text x="52.20" y="208.33" text-anchor="end" fill="#7f8c8d" font-size="11">Echo</text>
> <rect x="77.37" y="42.83" width="383.45" height="29.42" fill="#3498db"/>
> <rect x="77.37" y="79.61" width="313.31" height="29.42" fill="#3498db"/>
> <rect x="77.37" y="116.39" width="210.43" height="29.42" fill="#3498db"/>
> <rect x="77.37" y="153.17" width="154.32" height="29.42" fill="#3498db"/>
> <rect x="77.37" y="189.95" width="98.20" height="29.42" fill="#3498db"/>
> <text x="269.10" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Top 5 (horizontal)</text>
> <rect x="495.00" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511.00" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Stacked bar

Long-format data (one row per X × series) plus `PosStack`. The stack
position writes a `__ybase` column the bar geom reads as the bottom
edge of each rect.

> **Limitation**: all stack segments currently render in the layer's
> single color; per-segment coloring needs Phase 9 work.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

quarters = concat [replicate 3 q | q <- ["Q1", "Q2", "Q3", "Q4"]]
products = take 12 (cycle ["Widgets", "Gadgets", "Gizmos"])
sales = [12, 8, 4, 15, 10, 6, 18, 12, 8, 22, 14, 10] :: [Double]
df =
    fromColumns
        [ ("quarter", ColCat quarters)
        , ("product", ColCat products)
        , ("sales",   ColNum sales)
        ]
layer =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX     = Just (ColumnRef "quarter")
                , aesY     = Just (ColumnRef "sales")
                , aesGroup = Just (ColumnRef "product")
                , aesFill  = Just (ColumnRef "product")
                }
        , layerStat     = StatIdentity
        , layerPosition = PosStack
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Sales by quarter (stacked)"
        , chartSize = SizeChars 60 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 256" width="600" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="480" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="114.99" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q1</text>
> <text x="216.66" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q2</text>
> <text x="318.34" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q3</text>
> <text x="420.01" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q4</text>
> <text x="49" y="223.04" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="146.28" text-anchor="end" fill="#7f8c8d" font-size="11">20.0</text>
> <text x="49" y="69.52" text-anchor="end" fill="#7f8c8d" font-size="11">40.0</text>
> <rect x="74.32" y="173.32" width="81.34" height="46.06" fill="#3498db"/>
> <rect x="74.32" y="142.61" width="81.34" height="30.70" fill="#9b59b6"/>
> <rect x="74.32" y="127.26" width="81.34" height="15.35" fill="#1abc9c"/>
> <rect x="175.99" y="161.80" width="81.34" height="57.57" fill="#3498db"/>
> <rect x="175.99" y="123.42" width="81.34" height="38.38" fill="#9b59b6"/>
> <rect x="175.99" y="100.40" width="81.34" height="23.03" fill="#1abc9c"/>
> <rect x="277.67" y="150.29" width="81.34" height="69.08" fill="#3498db"/>
> <rect x="277.67" y="104.23" width="81.34" height="46.06" fill="#9b59b6"/>
> <rect x="277.67" y="73.53" width="81.34" height="30.70" fill="#1abc9c"/>
> <rect x="379.34" y="134.94" width="81.34" height="84.43" fill="#3498db"/>
> <rect x="379.34" y="81.21" width="81.34" height="53.73" fill="#9b59b6"/>
> <rect x="379.34" y="42.83" width="81.34" height="38.38" fill="#1abc9c"/>
> <text x="267.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Sales by quarter (stacked)</text>
> <rect x="495" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">Widgets</text>
> <rect x="495" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="511" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">Gadgets</text>
> <rect x="495" y="73" width="12" height="12" fill="#1abc9c"/>
> <text x="511" y="83" text-anchor="start" fill="#7f8c8d" font-size="11">Gizmos</text>
> </svg>

---

## Grouped (dodged) bar

Same data, swap `PosStack` for `PosDodge` to place groups side-by-side.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

quarters = concat [replicate 3 q | q <- ["Q1", "Q2", "Q3", "Q4"]]
products = take 12 (cycle ["Widgets", "Gadgets", "Gizmos"])
sales = [12, 8, 4, 15, 10, 6, 18, 12, 8, 22, 14, 10] :: [Double]
df =
    fromColumns
        [ ("quarter", ColCat quarters)
        , ("product", ColCat products)
        , ("sales",   ColNum sales)
        ]
layer =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX     = Just (ColumnRef "quarter")
                , aesY     = Just (ColumnRef "sales")
                , aesGroup = Just (ColumnRef "product")
                , aesFill  = Just (ColumnRef "product")
                }
        , layerStat     = StatIdentity
        , layerPosition = PosDodge 0.25
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Sales by quarter (grouped)"
        , chartSize = SizeChars 60 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 256" width="600" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="480" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="110.87" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q1</text>
> <text x="215.29" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q2</text>
> <text x="319.71" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q3</text>
> <text x="424.13" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q4</text>
> <text x="49" y="223.04" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="142.79" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="62.54" text-anchor="end" fill="#7f8c8d" font-size="11">20.0</text>
> <rect x="74.32" y="123.08" width="20.88" height="96.30" fill="#3498db"/>
> <rect x="100.42" y="155.17" width="20.88" height="64.20" fill="#9b59b6"/>
> <rect x="126.53" y="187.27" width="20.88" height="32.10" fill="#1abc9c"/>
> <rect x="178.74" y="99.00" width="20.88" height="120.37" fill="#3498db"/>
> <rect x="204.85" y="139.12" width="20.88" height="80.25" fill="#9b59b6"/>
> <rect x="230.95" y="171.22" width="20.88" height="48.15" fill="#1abc9c"/>
> <rect x="283.16" y="74.93" width="20.88" height="144.45" fill="#3498db"/>
> <rect x="309.27" y="123.08" width="20.88" height="96.30" fill="#9b59b6"/>
> <rect x="335.37" y="155.17" width="20.88" height="64.20" fill="#1abc9c"/>
> <rect x="387.59" y="42.83" width="20.88" height="176.55" fill="#3498db"/>
> <rect x="413.69" y="107.03" width="20.88" height="112.35" fill="#9b59b6"/>
> <rect x="439.80" y="139.12" width="20.88" height="80.25" fill="#1abc9c"/>
> <text x="267.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Sales by quarter (grouped)</text>
> <rect x="495" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">Widgets</text>
> <rect x="495" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="511" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">Gadgets</text>
> <rect x="495" y="73" width="12" height="12" fill="#1abc9c"/>
> <text x="511" y="83" text-anchor="start" fill="#7f8c8d" font-size="11">Gizmos</text>
> </svg>

---

## Pie

`GeomArc` reads `aesY` as slice values, normalises them, and emits
one wedge per row.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

df =
    fromColumns
        [ ("slice", ColCat ["A", "B", "C", "D"])
        , ("value", ColNum [40, 25, 20, 15])
        ]
layer =
    (defLayer GeomArc)
        { layerMapping = emptyMapping{ aesY = Just (ColumnRef "value") }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Pie"
        , chartSize = SizeChars 30 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 256" width="300" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="180" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="60.68" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="117.50" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.5</text>
> <text x="174.32" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="49" y="187.73" text-anchor="end" fill="#7f8c8d" font-size="11">20.0</text>
> <text x="49" y="117.11" text-anchor="end" fill="#7f8c8d" font-size="11">30.0</text>
> <text x="49" y="46.49" text-anchor="end" fill="#7f8c8d" font-size="11">40.0</text>
> <path d="M 117.50 131.10 L 117.50 74.85 A 56.25 56.25 0 0 0 150.56 176.61 Z" fill="#3498db" stroke="#7f8c8d" stroke-width="1"/>
> <path d="M 117.50 131.10 L 150.56 176.61 A 56.25 56.25 0 0 0 71.99 164.16 Z" fill="#9b59b6" stroke="#7f8c8d" stroke-width="1"/>
> <path d="M 117.50 131.10 L 71.99 164.16 A 56.25 56.25 0 0 0 71.99 98.04 Z" fill="#1abc9c" stroke="#7f8c8d" stroke-width="1"/>
> <path d="M 117.50 131.10 L 71.99 98.04 A 56.25 56.25 0 0 0 117.50 74.85 Z" fill="#2ecc71" stroke="#7f8c8d" stroke-width="1"/>
> <text x="117.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Pie</text>
> <rect x="195" y="39" width="12" height="12" fill="#3498db"/>
> <text x="211" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Filled area / ribbon

`GeomRibbon` fills between `aesYmin` and `aesYmax`. For an "area under
the curve" feel, set `ymin = 0`.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [0..12] :: [Double]
ymax = [sin (x/2) + 2 | x <- xs]
zeros = replicate (length xs) 0 :: [Double]
df =
    fromColumns
        [ ("x",    ColNum xs)
        , ("ymin", ColNum zeros)
        , ("ymax", ColNum ymax)
        ]
layer =
    (defLayer GeomRibbon)
        { layerMapping =
            emptyMapping
                { aesX    = Just (ColumnRef "x")
                , aesYmin = Just (ColumnRef "ymin")
                , aesYmax = Just (ColumnRef "ymax")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Filled area"
        , chartSize = SizeChars 56 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 224" width="560" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="440" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="72.50" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="218.33" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="364.17" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="143.30" text-anchor="end" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="49" y="94.11" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="44.92" text-anchor="end" fill="#7f8c8d" font-size="11">3.0</text>
> <polygon points="72.50,90.44 101.67,66.86 130.83,49.05 160.00,41.37 189.17,45.71 218.33,61.00 247.50,83.50 276.67,107.70 305.83,127.67 335.00,138.53 364.17,137.61 393.33,125.15 422.50,104.19 422.50,188.83 393.33,188.83 364.17,188.83 335.00,188.83 305.83,188.83 276.67,188.83 247.50,188.83 218.33,188.83 189.17,188.83 160.00,188.83 130.83,188.83 101.67,188.83 72.50,188.83" fill="#3498db" stroke="#3498db" stroke-width="1" fill-opacity="0.40"/>
> <text x="247.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Filled area</text>
> <rect x="455" y="39" width="12" height="12" fill="#3498db"/>
> <text x="471" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Histogram

`GeomHistogram` + `StatBin` buckets numeric X and writes counts into
a `count` column. Set `aesY = Just (ColumnRef "count")` to surface
that column to the bar geom.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

raw = take 50 (cycle [1.0, 1.2, 1.8, 2.0, 2.3, 2.7, 3.1, 3.4, 3.8, 4.2])
df = fromColumns [("x", ColNum raw)]
layer =
    (defLayer GeomHistogram)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "count")
                }
        , layerStat = StatBin (BinByCount 8)
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Histogram"
        , chartSize = SizeChars 44 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 440 224" width="440" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="320" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="63.96" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="141.17" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="218.39" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">3.0</text>
> <text x="295.60" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="118.77" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="49" y="45.04" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <rect x="67.05" y="41.37" width="24.71" height="147.45" fill="#3498db"/>
> <rect x="97.93" y="188.83" width="24.71" height="0" fill="#3498db"/>
> <rect x="128.82" y="41.37" width="24.71" height="147.45" fill="#3498db"/>
> <rect x="159.70" y="115.10" width="24.71" height="73.73" fill="#3498db"/>
> <rect x="190.59" y="115.10" width="24.71" height="73.73" fill="#3498db"/>
> <rect x="221.47" y="115.10" width="24.71" height="73.73" fill="#3498db"/>
> <rect x="252.36" y="115.10" width="24.71" height="73.73" fill="#3498db"/>
> <rect x="283.25" y="41.37" width="24.71" height="147.45" fill="#3498db"/>
> <text x="187.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Histogram</text>
> <rect x="335" y="39" width="12" height="12" fill="#3498db"/>
> <text x="351" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Box plot

`StatBoxplot` computes a five-number summary (Tukey hinges) per
group. The geom reads `__ymin` / `__q1` / `__median` / `__q3` / `__ymax`
and draws the box, median line, and whiskers.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

pts =
    [("A", v) | v <- [1..9 :: Double]]
        <> [("B", v) | v <- [3..11]]
        <> [("C", v) | v <- [5..13]]
df =
    fromColumns
        [ ("g", ColCat [g | (g, _) <- pts])
        , ("y", ColNum [v | (_, v) <- pts])
        ]
layer =
    (defLayer GeomBoxplot)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "g")
                , aesY = Just (ColumnRef "y")
                }
        , layerStat = StatBoxplot
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Boxplot"
        , chartSize = SizeChars 40 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 256" width="400" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="280" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="94.45" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">A</text>
> <text x="167.50" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">B</text>
> <text x="240.55" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">C</text>
> <text x="49" y="164.19" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="49" y="90.63" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <polygon points="65.23,189.95 123.67,189.95 123.67,131.10 65.23,131.10" fill="#3498db" stroke="#3498db" stroke-width="1" fill-opacity="0.20"/>
> <polyline points="65.23,160.52 123.67,160.52" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="94.45,189.95 94.45,219.37" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="94.45,131.10 94.45,101.68" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <polygon points="138.28,160.52 196.72,160.52 196.72,101.68 138.28,101.68" fill="#3498db" stroke="#3498db" stroke-width="1" fill-opacity="0.20"/>
> <polyline points="138.28,131.10 196.72,131.10" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,160.52 167.50,189.95" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,101.68 167.50,72.25" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <polygon points="211.33,131.10 269.77,131.10 269.77,72.25 211.33,72.25" fill="#3498db" stroke="#3498db" stroke-width="1" fill-opacity="0.20"/>
> <polyline points="211.33,101.68 269.77,101.68" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="240.55,131.10 240.55,160.52" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="240.55,72.25 240.55,42.83" fill="none" stroke="#3498db" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="167.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Boxplot</text>
> <rect x="295" y="39" width="12" height="12" fill="#3498db"/>
> <text x="311" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Error bars

`GeomErrorbar` reads `aesYmin` / `aesYmax` and draws capped vertical
lines. Layer it under a `GeomPoint` to show the central estimate.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [1..5] :: [Double]
ys = [2.1, 3.5, 5.2, 4.7, 6.1] :: [Double]
los = [1.5, 2.8, 4.6, 4.0, 5.3] :: [Double]
his = [2.7, 4.2, 5.8, 5.4, 6.9] :: [Double]
df =
    fromColumns
        [ ("x",  ColNum xs)
        , ("y",  ColNum ys)
        , ("lo", ColNum los)
        , ("hi", ColNum his)
        ]
m =
    emptyMapping
        { aesX    = Just (ColumnRef "x")
        , aesY    = Just (ColumnRef "y")
        , aesYmin = Just (ColumnRef "lo")
        , aesYmax = Just (ColumnRef "hi")
        }
bars = (defLayer GeomErrorbar){ layerMapping = m, layerStat = StatIdentity }
pts = (defLayer GeomPoint)   { layerMapping = m, layerStat = StatIdentity }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [bars, pts]
        , chartTitle = Just "Measurements +/- CI"
        , chartSize = SizeChars 56 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 224" width="560" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="440" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="174.58" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="320.42" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="178.84" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="124.23" text-anchor="end" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="69.62" text-anchor="end" fill="#7f8c8d" font-size="11">6.0</text>
> <polyline points="101.67,188.83 101.67,156.06" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="90.00,188.83 113.33,188.83" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="90.00,156.06 113.33,156.06" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="174.58,153.33 174.58,115.10" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="162.92,153.33 186.25,153.33" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="162.92,115.10 186.25,115.10" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="247.50,104.18 247.50,71.41" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="235.83,104.18 259.17,104.18" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="235.83,71.41 259.17,71.41" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="320.42,120.56 320.42,82.33" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="308.75,120.56 332.08,120.56" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="308.75,82.33 332.08,82.33" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="393.33,85.06 393.33,41.37" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="381.67,85.06 405.00,85.06" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="381.67,41.37 405.00,41.37" fill="none" stroke="#3498db" stroke-width="1.50" stroke-linejoin="round" stroke-linecap="round"/>
> <circle cx="101.67" cy="172.44" r="3" fill="#9b59b6"/>
> <circle cx="174.58" cy="134.21" r="3" fill="#9b59b6"/>
> <circle cx="247.50" cy="87.79" r="3" fill="#9b59b6"/>
> <circle cx="320.42" cy="101.45" r="3" fill="#9b59b6"/>
> <circle cx="393.33" cy="63.22" r="3" fill="#9b59b6"/>
> <text x="247.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Measurements +/- CI</text>
> <rect x="455" y="39" width="12" height="12" fill="#3498db"/>
> <text x="471" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="455" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="471" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

---

## Density (KDE)

`StatDensity` runs a Gaussian kernel density estimate (Silverman
bandwidth) and writes the curve into a `density` column.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

sample =
    [ 1.0, 1.2, 1.3, 1.5, 1.7, 2.0, 2.1, 2.3, 2.5
    , 2.8, 3.0, 3.1, 3.3, 3.7, 3.9, 4.0
    ]
df = fromColumns [("x", ColNum sample)]
layer =
    (defLayer GeomDensity)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "density")
                }
        , layerStat = StatDensity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Density (KDE)"
        , chartSize = SizeChars 56 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 224" width="560" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="440" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="114.60" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="220.92" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="327.24" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="433.57" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="192.86" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="144.33" text-anchor="end" fill="#7f8c8d" font-size="11">0.1</text>
> <text x="49" y="95.80" text-anchor="end" fill="#7f8c8d" font-size="11">0.2</text>
> <text x="49" y="47.27" text-anchor="end" fill="#7f8c8d" font-size="11">0.3</text>
> <polyline points="72.50,188.83 75.26,188.71 78.01,188.57 80.77,188.39 83.52,188.16 86.28,187.87 89.04,187.51 91.79,187.08 94.55,186.55 97.30,185.91 100.06,185.15 102.81,184.24 105.57,183.17 108.33,181.91 111.08,180.45 113.84,178.77 116.59,176.84 119.35,174.66 122.11,172.20 124.86,169.45 127.62,166.40 130.37,163.05 133.13,159.40 135.89,155.45 138.64,151.20 141.40,146.68 144.15,141.90 146.91,136.90 149.67,131.70 152.42,126.35 155.18,120.87 157.93,115.32 160.69,109.75 163.44,104.19 166.20,98.70 168.96,93.32 171.71,88.10 174.47,83.08 177.22,78.29 179.98,73.76 182.74,69.52 185.49,65.59 188.25,61.98 191.00,58.70 193.76,55.75 196.52,53.13 199.27,50.82 202.03,48.82 204.78,47.11 207.54,45.67 210.30,44.48 213.05,43.51 215.81,42.76 218.56,42.19 221.32,41.79 224.07,41.53 226.83,41.39 229.59,41.37 232.34,41.45 235.10,41.60 237.85,41.84 240.61,42.13 243.37,42.49 246.12,42.91 248.88,43.38 251.63,43.91 254.39,44.49 257.15,45.14 259.90,45.86 262.66,46.65 265.41,47.52 268.17,48.48 270.93,49.54 273.68,50.70 276.44,51.98 279.19,53.39 281.95,54.92 284.70,56.61 287.46,58.44 290.22,60.43 292.97,62.60 295.73,64.94 298.48,67.46 301.24,70.18 304.00,73.10 306.75,76.21 309.51,79.53 312.26,83.04 315.02,86.75 317.78,90.65 320.53,94.72 323.29,98.96 326.04,103.33 328.80,107.83 331.56,112.42 334.31,117.07 337.07,121.76 339.82,126.46 342.58,131.12 345.33,135.71 348.09,140.21 350.85,144.57 353.60,148.78 356.36,152.81 359.11,156.62 361.87,160.22 364.63,163.58 367.38,166.69 370.14,169.54 372.89,172.15 375.65,174.51 378.41,176.62 381.16,178.50 383.92,180.16 386.67,181.62 389.43,182.88 392.19,183.97 394.94,184.90 397.70,185.69 400.45,186.35 403.21,186.91 405.96,187.37 408.72,187.74 411.48,188.05 414.23,188.30 416.99,188.50 419.74,188.66 422.50,188.78" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="247.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Density (KDE)</text>
> <rect x="455" y="39" width="12" height="12" fill="#3498db"/>
> <text x="471" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Distplot (histogram + density)

Two layers on shared axes — counts and density share a Y range, so
expect one to look small relative to the other.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

sample =
    [ 1.0, 1.2, 1.3, 1.5, 1.7, 2.0, 2.1, 2.3, 2.5
    , 2.8, 3.0, 3.1, 3.3, 3.7, 3.9, 4.0
    ]
df = fromColumns [("x", ColNum sample)]
hist =
    (defLayer GeomHistogram)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "count")
                }
        , layerStat = StatBin (BinByCount 8)
        }
kde =
    (defLayer GeomDensity)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "density")
                }
        , layerStat = StatDensity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [hist, kde]
        , chartTitle = Just "Distribution"
        , chartSize = SizeChars 56 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 256" width="560" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="440" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="120.39" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="222.08" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="323.77" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="425.46" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="223.04" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="164.19" text-anchor="end" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="49" y="105.34" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="46.49" text-anchor="end" fill="#7f8c8d" font-size="11">3.0</text>
> <rect x="173.14" y="42.83" width="15.25" height="176.55" fill="#3498db"/>
> <rect x="192.21" y="101.68" width="15.25" height="117.70" fill="#3498db"/>
> <rect x="211.27" y="101.68" width="15.25" height="117.70" fill="#3498db"/>
> <rect x="230.34" y="160.52" width="15.25" height="58.85" fill="#3498db"/>
> <rect x="249.41" y="101.68" width="15.25" height="117.70" fill="#3498db"/>
> <rect x="268.47" y="101.68" width="15.25" height="117.70" fill="#3498db"/>
> <rect x="287.54" y="160.52" width="15.25" height="58.85" fill="#3498db"/>
> <rect x="306.61" y="42.83" width="15.25" height="176.55" fill="#3498db"/>
> <polyline points="80.13,219.33 82.76,219.31 85.40,219.30 88.03,219.28 90.67,219.25 93.31,219.21 95.94,219.17 98.58,219.12 101.21,219.05 103.85,218.98 106.48,218.88 109.12,218.77 111.76,218.64 114.39,218.49 117.03,218.31 119.66,218.11 122.30,217.88 124.94,217.61 127.57,217.31 130.21,216.98 132.84,216.61 135.48,216.20 138.11,215.76 140.75,215.28 143.39,214.77 146.02,214.22 148.66,213.64 151.29,213.03 153.93,212.40 156.56,211.75 159.20,211.09 161.84,210.42 164.47,209.74 167.11,209.06 169.74,208.40 172.38,207.75 175.02,207.11 177.65,206.50 180.29,205.92 182.92,205.38 185.56,204.86 188.19,204.38 190.83,203.95 193.47,203.55 196.10,203.19 198.74,202.87 201.37,202.59 204.01,202.35 206.65,202.14 209.28,201.97 211.92,201.82 214.55,201.71 217.19,201.62 219.82,201.55 222.46,201.50 225.10,201.47 227.73,201.45 230.37,201.45 233.00,201.46 235.64,201.48 238.27,201.50 240.91,201.54 243.55,201.58 246.18,201.63 248.82,201.69 251.45,201.76 254.09,201.83 256.73,201.91 259.36,201.99 262.00,202.09 264.63,202.19 267.27,202.31 269.90,202.44 272.54,202.58 275.18,202.73 277.81,202.90 280.45,203.09 283.08,203.29 285.72,203.52 288.35,203.76 290.99,204.02 293.63,204.31 296.26,204.61 298.90,204.94 301.53,205.29 304.17,205.67 306.81,206.07 309.44,206.50 312.08,206.95 314.71,207.42 317.35,207.92 319.98,208.43 322.62,208.96 325.26,209.51 327.89,210.06 330.53,210.63 333.16,211.20 335.80,211.77 338.44,212.33 341.07,212.89 343.71,213.43 346.34,213.96 348.98,214.47 351.61,214.96 354.25,215.42 356.89,215.86 359.52,216.27 362.16,216.64 364.79,216.99 367.43,217.31 370.06,217.59 372.70,217.85 375.34,218.08 377.97,218.28 380.61,218.45 383.24,218.61 385.88,218.74 388.52,218.85 391.15,218.95 393.79,219.03 396.42,219.10 399.06,219.15 401.69,219.20 404.33,219.23 406.97,219.26 409.60,219.29 412.24,219.31 414.87,219.32" fill="none" stroke="#9b59b6" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="247.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Distribution</text>
> <rect x="455" y="39" width="12" height="12" fill="#3498db"/>
> <text x="471" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="455" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="471" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

---

## Gauss (z-score distribution)

`gauss` is a higher-level helper (it returns SVG directly rather than a
`Chart`). Give it a **population** to fix the mean (μ) and standard
deviation (σ), plus a list of named **markers**. It draws the kernel
density estimate as a stippled bell curve, labels the x-axis in σ units,
and drops a lollipop annotation onto each marker at its z-score — the
largest one highlighted as the outlier. Think "where does this player
sit on the curve?".

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import qualified Granite.Svg as G

-- A synthetic, right-skewed "every attacker" population: goals + assists
-- per 90. Most players cluster low; a thin tail of elite finishers.
population =
    concat
        [ replicate count value
        | (count, value) <-
            [ (40, 0.05), (90, 0.12), (120, 0.20), (110, 0.28)
            , (80, 0.36), (55, 0.45), (35, 0.55), (20, 0.66)
            , (10, 0.78), (6, 0.90), (3, 1.02)
            ]
        ]
stars =
    [ ("Lewandowski", 0.95)
    , ("Mbappe",      0.92)
    , ("Haaland",     1.05)
    , ("Ronaldo",     1.10)
    , ("Messi",       1.45)
    ]
chart =
    G.gauss population stars
        G.defPlot
            { G.widthChars = 72
            , G.heightChars = 22
            , G.plotTitle = "every attacker, top-5 leagues - goals + assists per 90"
            }

Text.IO.putStrLn chart
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 910 428" width="910" height="428" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="430" y="26" text-anchor="middle" fill="#222" font-size="14">every attacker, top-5 leagues - goals + assists per 90</text>
> <path d="M 70 386 L 70 363.15 L 73.33 357.56 L 76.67 351.09 L 80.00 343.73 L 83.33 335.48 L 86.67 326.38 L 90.00 316.47 L 93.33 305.82 L 96.67 294.52 L 100.00 282.68 L 103.33 270.40 L 106.67 257.82 L 110.00 245.05 L 113.33 232.23 L 116.67 219.48 L 120.00 206.92 L 123.33 194.68 L 126.67 182.85 L 130.00 171.51 L 133.33 160.73 L 136.67 150.52 L 140.00 140.92 L 143.33 131.90 L 146.67 123.46 L 150.00 115.56 L 153.33 108.20 L 156.67 101.36 L 160.00 95.07 L 163.33 89.35 L 166.67 84.26 L 170.00 79.84 L 173.33 76.15 L 176.67 73.23 L 180.00 71.10 L 183.33 69.76 L 186.67 69.20 L 190.00 69.38 L 193.33 70.27 L 196.67 71.81 L 200.00 73.98 L 203.33 76.75 L 206.67 80.09 L 210.00 84.01 L 213.33 88.48 L 216.67 93.50 L 220.00 99.03 L 223.33 105.04 L 226.67 111.49 L 230.00 118.32 L 233.33 125.48 L 236.67 132.91 L 240.00 140.55 L 243.33 148.35 L 246.67 156.25 L 250.00 164.21 L 253.33 172.15 L 256.67 180.03 L 260.00 187.77 L 263.33 195.32 L 266.67 202.62 L 270.00 209.62 L 273.33 216.32 L 276.67 222.71 L 280.00 228.81 L 283.33 234.67 L 286.67 240.32 L 290.00 245.83 L 293.33 251.22 L 296.67 256.52 L 300.00 261.71 L 303.33 266.79 L 306.67 271.70 L 310.00 276.41 L 313.33 280.86 L 316.67 285.05 L 320.00 288.95 L 323.33 292.58 L 326.67 295.99 L 330.00 299.23 L 333.33 302.37 L 336.67 305.47 L 340.00 308.57 L 343.33 311.70 L 346.67 314.87 L 350.00 318.03 L 353.33 321.16 L 356.67 324.20 L 360.00 327.08 L 363.33 329.76 L 366.67 332.21 L 370.00 334.42 L 373.33 336.40 L 376.67 338.19 L 380.00 339.86 L 383.33 341.45 L 386.67 343.02 L 390.00 344.64 L 393.33 346.33 L 396.67 348.10 L 400.00 349.93 L 403.33 351.81 L 406.67 353.68 L 410.00 355.49 L 413.33 357.18 L 416.67 358.73 L 420.00 360.10 L 423.33 361.28 L 426.67 362.27 L 430.00 363.11 L 433.33 363.82 L 436.67 364.45 L 440.00 365.04 L 443.33 365.63 L 446.67 366.25 L 450.00 366.91 L 453.33 367.61 L 456.67 368.35 L 460.00 369.11 L 463.33 369.86 L 466.67 370.56 L 470.00 371.21 L 473.33 371.79 L 476.67 372.27 L 480.00 372.67 L 483.33 373.00 L 486.67 373.28 L 490.00 373.52 L 493.33 373.76 L 496.67 374.02 L 500.00 374.31 L 503.33 374.65 L 506.67 375.05 L 510.00 375.49 L 513.33 375.96 L 516.67 376.45 L 520.00 376.95 L 523.33 377.42 L 526.67 377.87 L 530.00 378.27 L 533.33 378.62 L 536.67 378.93 L 540.00 379.20 L 543.33 379.44 L 546.67 379.66 L 550.00 379.89 L 553.33 380.13 L 556.67 380.39 L 560.00 380.69 L 563.33 381.03 L 566.67 381.40 L 570.00 381.80 L 573.33 382.21 L 576.67 382.64 L 580.00 383.07 L 583.33 383.48 L 586.67 383.87 L 590.00 384.23 L 593.33 384.56 L 596.67 384.84 L 600.00 385.09 L 603.33 385.29 L 606.67 385.46 L 610.00 385.60 L 613.33 385.70 L 616.67 385.79 L 620.00 385.85 L 623.33 385.89 L 626.67 385.93 L 630.00 385.95 L 633.33 385.97 L 636.67 385.98 L 640.00 385.99 L 643.33 385.99 L 646.67 385.99 L 650.00 386.00 L 653.33 386.00 L 656.67 386.00 L 660.00 386.00 L 663.33 386.00 L 666.67 386.00 L 670.00 386.00 L 673.33 386.00 L 676.67 386.00 L 680.00 386.00 L 683.33 386.00 L 686.67 386.00 L 690.00 386.00 L 693.33 386.00 L 696.67 386.00 L 700.00 386.00 L 703.33 386.00 L 706.67 386.00 L 710.00 386.00 L 713.33 386.00 L 716.67 386.00 L 720.00 386.00 L 723.33 386.00 L 726.67 386.00 L 730.00 386.00 L 733.33 386.00 L 736.67 386.00 L 740.00 386.00 L 743.33 386.00 L 746.67 386.00 L 750.00 386.00 L 753.33 386.00 L 756.67 386 L 760.00 386 L 763.33 386 L 766.67 386 L 770.00 386 L 773.33 386 L 776.67 386 L 780.00 386 L 783.33 386 L 786.67 386 L 790.00 386 L 790.00 386 Z" fill="#dfe4e8" fill-opacity="0.6"/>
> <line x1="222.69" y1="34" x2="222.69" y2="386" stroke="#cfd6dc" stroke-width="1" stroke-dasharray="3 4"/>
> <circle cx="78.95" cy="368.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="79.70" cy="371.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="89.27" cy="342.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="80.02" cy="363.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="85.39" cy="380.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="99.59" cy="305.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="90.34" cy="316.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="95.07" cy="329.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="97.18" cy="375.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="100.66" cy="285.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="104.75" cy="305.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="104.00" cy="322.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="103.25" cy="334.82" r="1.30" fill="#aeb4bb"/>
> <circle cx="107.50" cy="348.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="108.25" cy="364.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="107.16" cy="383.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="110.23" cy="237.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="110.98" cy="250.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="114.43" cy="261.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="117.07" cy="297.40" r="1.30" fill="#aeb4bb"/>
> <circle cx="118.57" cy="317.93" r="1.30" fill="#aeb4bb"/>
> <circle cx="116.84" cy="330.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="116.09" cy="355.93" r="1.30" fill="#aeb4bb"/>
> <circle cx="114.66" cy="358.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="115.41" cy="377.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="121.30" cy="216.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="123.36" cy="234.94" r="1.30" fill="#aeb4bb"/>
> <circle cx="127.39" cy="248.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="128.14" cy="272.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="128.89" cy="273.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="125.77" cy="310.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="124.98" cy="326.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="125.73" cy="339.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="128.93" cy="365.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="128.18" cy="385.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="134.54" cy="173.67" r="1.30" fill="#aeb4bb"/>
> <circle cx="133.79" cy="196.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="133.04" cy="207.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="137.71" cy="220.25" r="1.30" fill="#aeb4bb"/>
> <circle cx="136.95" cy="254.18" r="1.30" fill="#aeb4bb"/>
> <circle cx="135.45" cy="276.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="135.30" cy="286.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="136.05" cy="313.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="136.80" cy="316.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="138.61" cy="333.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="137.86" cy="346.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="132.89" cy="370.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="141.19" cy="136.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="144.22" cy="149.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="142.72" cy="183.41" r="1.30" fill="#aeb4bb"/>
> <circle cx="148.78" cy="204.50" r="1.30" fill="#aeb4bb"/>
> <circle cx="146.63" cy="215.85" r="1.30" fill="#aeb4bb"/>
> <circle cx="145.88" cy="228.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="145.13" cy="244.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="146.37" cy="274.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="149.04" cy="297.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="148.29" cy="308.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="143.21" cy="337.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="143.96" cy="355.17" r="1.30" fill="#aeb4bb"/>
> <circle cx="141.45" cy="367.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="140.70" cy="377.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="151.51" cy="117.85" r="1.30" fill="#aeb4bb"/>
> <circle cx="153.90" cy="121.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="152.40" cy="149.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="158.35" cy="174.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="159.10" cy="176.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="156.31" cy="194.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="154.81" cy="228.91" r="1.30" fill="#aeb4bb"/>
> <circle cx="156.69" cy="252.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="158.72" cy="267.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="157.97" cy="286.12" r="1.30" fill="#aeb4bb"/>
> <circle cx="157.22" cy="298.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="153.53" cy="306.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="151.13" cy="330.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="159.63" cy="363.25" r="1.30" fill="#aeb4bb"/>
> <circle cx="151.12" cy="375.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="161.83" cy="89.87" r="1.30" fill="#aeb4bb"/>
> <circle cx="163.58" cy="113.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="162.83" cy="115.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.92" cy="136.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="169.42" cy="169.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.99" cy="182.13" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.24" cy="192.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.51" cy="208.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="166.26" cy="228.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.01" cy="231.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.65" cy="260.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="163.10" cy="285.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="163.85" cy="287.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="160.81" cy="324.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="160.06" cy="340.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="160.69" cy="352.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="161.44" cy="364.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="162.19" cy="380.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="173.26" cy="97.63" r="1.30" fill="#aeb4bb"/>
> <circle cx="172.51" cy="113.54" r="1.30" fill="#aeb4bb"/>
> <circle cx="178.24" cy="127.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="178.99" cy="136.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="179.74" cy="152.63" r="1.30" fill="#aeb4bb"/>
> <circle cx="175.67" cy="173.29" r="1.30" fill="#aeb4bb"/>
> <circle cx="174.92" cy="175.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="175.83" cy="191.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="177.33" cy="228.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="177.33" cy="250.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="173.42" cy="267.99" r="1.30" fill="#aeb4bb"/>
> <circle cx="174.17" cy="283.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="174.92" cy="296.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="170.49" cy="307.08" r="1.30" fill="#aeb4bb"/>
> <circle cx="171.01" cy="342.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="172.51" cy="362.08" r="1.30" fill="#aeb4bb"/>
> <circle cx="172.90" cy="374.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="183.69" cy="80.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.94" cy="88.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.19" cy="100.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="189.31" cy="128.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.10" cy="146.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="184.60" cy="182.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.15" cy="193.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.90" cy="206.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="188.51" cy="222.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="187.76" cy="240.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="187.01" cy="252.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="184.49" cy="272.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="180.92" cy="299.19" r="1.30" fill="#aeb4bb"/>
> <circle cx="180.17" cy="301.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="181.33" cy="331.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.08" cy="355.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.83" cy="365.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.58" cy="378.29" r="1.30" fill="#aeb4bb"/>
> <circle cx="192.62" cy="86.40" r="1.30" fill="#aeb4bb"/>
> <circle cx="198.88" cy="118.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.63" cy="139.99" r="1.30" fill="#aeb4bb"/>
> <circle cx="195.78" cy="152.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="195.03" cy="164.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.28" cy="179.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="196.47" cy="198.33" r="1.30" fill="#aeb4bb"/>
> <circle cx="197.22" cy="210.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="198.19" cy="219.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="196.69" cy="243.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.06" cy="258.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.81" cy="277.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="190.60" cy="289.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.85" cy="312.34" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.10" cy="323.34" r="1.30" fill="#aeb4bb"/>
> <circle cx="191.65" cy="335.91" r="1.30" fill="#aeb4bb"/>
> <circle cx="193.01" cy="369.67" r="1.30" fill="#aeb4bb"/>
> <circle cx="202.30" cy="104.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="208.45" cy="117.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.20" cy="128.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.95" cy="144.95" r="1.30" fill="#aeb4bb"/>
> <circle cx="204.71" cy="169.12" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.54" cy="223.70" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.87" cy="226.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.12" cy="245.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="206.37" cy="264.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="204.38" cy="280.67" r="1.30" fill="#aeb4bb"/>
> <circle cx="205.13" cy="293.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.53" cy="321.25" r="1.30" fill="#aeb4bb"/>
> <circle cx="208.78" cy="340.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="201.97" cy="353.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="202.69" cy="373.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="212.73" cy="96.33" r="1.30" fill="#aeb4bb"/>
> <circle cx="211.98" cy="112.13" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.52" cy="135.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="215.14" cy="164.33" r="1.30" fill="#aeb4bb"/>
> <circle cx="214.39" cy="187.88" r="1.30" fill="#aeb4bb"/>
> <circle cx="216.36" cy="189.69" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.11" cy="209.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.86" cy="226.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.55" cy="242.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="216.80" cy="255.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="213.95" cy="265.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="215.45" cy="300.91" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.96" cy="304.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.21" cy="320.03" r="1.30" fill="#aeb4bb"/>
> <circle cx="211.54" cy="332.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="212.29" cy="357.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="213.04" cy="358.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="212.37" cy="377.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="221.66" cy="128.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="229.84" cy="164.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="220.59" cy="175.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.82" cy="188.67" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.07" cy="204.95" r="1.30" fill="#aeb4bb"/>
> <circle cx="227.43" cy="235.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="226.48" cy="283.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.27" cy="285.93" r="1.30" fill="#aeb4bb"/>
> <circle cx="225.02" cy="302.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="225.77" cy="316.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="228.89" cy="342.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="222.61" cy="380.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.09" cy="140.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="231.34" cy="152.91" r="1.30" fill="#aeb4bb"/>
> <circle cx="239.41" cy="165.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="230.16" cy="180.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="234.50" cy="211.61" r="1.30" fill="#aeb4bb"/>
> <circle cx="233.75" cy="219.70" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.00" cy="231.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.75" cy="243.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.66" cy="259.13" r="1.30" fill="#aeb4bb"/>
> <circle cx="236.91" cy="277.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="234.59" cy="312.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="235.34" cy="323.63" r="1.30" fill="#aeb4bb"/>
> <circle cx="230.07" cy="336.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="238.57" cy="369.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.18" cy="382.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="241.02" cy="180.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="240.48" cy="200.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.93" cy="213.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.18" cy="226.51" r="1.30" fill="#aeb4bb"/>
> <circle cx="243.43" cy="241.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="247.32" cy="260.85" r="1.30" fill="#aeb4bb"/>
> <circle cx="248.07" cy="273.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="246.59" cy="308.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.91" cy="336.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="245.66" cy="355.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="249.75" cy="368.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="249.00" cy="376.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="251.45" cy="180.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="259.30" cy="194.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="250.05" cy="219.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="254.61" cy="243.12" r="1.30" fill="#aeb4bb"/>
> <circle cx="253.86" cy="260.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="257.64" cy="291.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="258.39" cy="302.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="257.02" cy="319.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="256.27" cy="339.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="254.48" cy="343.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="255.23" cy="360.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="269.62" cy="220.68" r="1.30" fill="#aeb4bb"/>
> <circle cx="261.12" cy="252.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="264.29" cy="267.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="263.54" cy="276.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="267.21" cy="292.53" r="1.30" fill="#aeb4bb"/>
> <circle cx="267.96" cy="313.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="268.71" cy="316.17" r="1.30" fill="#aeb4bb"/>
> <circle cx="266.70" cy="332.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="265.95" cy="346.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="265.55" cy="372.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="279.94" cy="246.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="270.69" cy="255.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="274.72" cy="267.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="273.97" cy="281.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="277.53" cy="316.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="278.28" cy="329.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="276.38" cy="365.19" r="1.30" fill="#aeb4bb"/>
> <circle cx="275.63" cy="378.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="280.49" cy="240.63" r="1.30" fill="#aeb4bb"/>
> <circle cx="280.26" cy="253.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="284.40" cy="288.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="282.90" cy="323.62" r="1.30" fill="#aeb4bb"/>
> <circle cx="287.85" cy="336.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="288.60" cy="349.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="286.81" cy="364.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="286.06" cy="384.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="290.58" cy="278.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="294.08" cy="308.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="293.33" cy="318.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="292.58" cy="335.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="298.17" cy="355.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="296.49" cy="375.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="300.15" cy="280.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="300.90" cy="289.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="301.65" cy="305.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="303.76" cy="327.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="307.74" cy="346.12" r="1.30" fill="#aeb4bb"/>
> <circle cx="308.49" cy="360.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="309.24" cy="384.18" r="1.30" fill="#aeb4bb"/>
> <circle cx="311.22" cy="299.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="313.44" cy="331.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="312.69" cy="354.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="318.06" cy="370.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="318.81" cy="378.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="320.79" cy="299.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="321.54" cy="321.62" r="1.30" fill="#aeb4bb"/>
> <circle cx="323.87" cy="335.01" r="1.30" fill="#aeb4bb"/>
> <circle cx="323.12" cy="348.54" r="1.30" fill="#aeb4bb"/>
> <circle cx="322.37" cy="364.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="328.38" cy="384.23" r="1.30" fill="#aeb4bb"/>
> <circle cx="331.86" cy="329.01" r="1.30" fill="#aeb4bb"/>
> <circle cx="332.80" cy="366.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="342.18" cy="331.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="343.23" cy="345.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="342.48" cy="369.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="348.27" cy="371.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="351.75" cy="335.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="352.50" cy="353.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="352.16" cy="379.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="362.82" cy="364.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="372.39" cy="342.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="373.02" cy="357.50" r="1.30" fill="#aeb4bb"/>
> <circle cx="372.27" cy="371.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="382.71" cy="342.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="382.70" cy="363.88" r="1.30" fill="#aeb4bb"/>
> <circle cx="393.03" cy="360.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="392.38" cy="379.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="402.06" cy="371.29" r="1.30" fill="#aeb4bb"/>
> <circle cx="411.74" cy="376.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="422.17" cy="377.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="441.53" cy="373.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="451.21" cy="377.94" r="1.30" fill="#aeb4bb"/>
> <circle cx="460.89" cy="382.98" r="1.30" fill="#aeb4bb"/>
> <polyline points="70,363.15 73.33,357.56 76.67,351.09 80.00,343.73 83.33,335.48 86.67,326.38 90.00,316.47 93.33,305.82 96.67,294.52 100.00,282.68 103.33,270.40 106.67,257.82 110.00,245.05 113.33,232.23 116.67,219.48 120.00,206.92 123.33,194.68 126.67,182.85 130.00,171.51 133.33,160.73 136.67,150.52 140.00,140.92 143.33,131.90 146.67,123.46 150.00,115.56 153.33,108.20 156.67,101.36 160.00,95.07 163.33,89.35 166.67,84.26 170.00,79.84 173.33,76.15 176.67,73.23 180.00,71.10 183.33,69.76 186.67,69.20 190.00,69.38 193.33,70.27 196.67,71.81 200.00,73.98 203.33,76.75 206.67,80.09 210.00,84.01 213.33,88.48 216.67,93.50 220.00,99.03 223.33,105.04 226.67,111.49 230.00,118.32 233.33,125.48 236.67,132.91 240.00,140.55 243.33,148.35 246.67,156.25 250.00,164.21 253.33,172.15 256.67,180.03 260.00,187.77 263.33,195.32 266.67,202.62 270.00,209.62 273.33,216.32 276.67,222.71 280.00,228.81 283.33,234.67 286.67,240.32 290.00,245.83 293.33,251.22 296.67,256.52 300.00,261.71 303.33,266.79 306.67,271.70 310.00,276.41 313.33,280.86 316.67,285.05 320.00,288.95 323.33,292.58 326.67,295.99 330.00,299.23 333.33,302.37 336.67,305.47 340.00,308.57 343.33,311.70 346.67,314.87 350.00,318.03 353.33,321.16 356.67,324.20 360.00,327.08 363.33,329.76 366.67,332.21 370.00,334.42 373.33,336.40 376.67,338.19 380.00,339.86 383.33,341.45 386.67,343.02 390.00,344.64 393.33,346.33 396.67,348.10 400.00,349.93 403.33,351.81 406.67,353.68 410.00,355.49 413.33,357.18 416.67,358.73 420.00,360.10 423.33,361.28 426.67,362.27 430.00,363.11 433.33,363.82 436.67,364.45 440.00,365.04 443.33,365.63 446.67,366.25 450.00,366.91 453.33,367.61 456.67,368.35 460.00,369.11 463.33,369.86 466.67,370.56 470.00,371.21 473.33,371.79 476.67,372.27 480.00,372.67 483.33,373.00 486.67,373.28 490.00,373.52 493.33,373.76 496.67,374.02 500.00,374.31 503.33,374.65 506.67,375.05 510.00,375.49 513.33,375.96 516.67,376.45 520.00,376.95 523.33,377.42 526.67,377.87 530.00,378.27 533.33,378.62 536.67,378.93 540.00,379.20 543.33,379.44 546.67,379.66 550.00,379.89 553.33,380.13 556.67,380.39 560.00,380.69 563.33,381.03 566.67,381.40 570.00,381.80 573.33,382.21 576.67,382.64 580.00,383.07 583.33,383.48 586.67,383.87 590.00,384.23 593.33,384.56 596.67,384.84 600.00,385.09 603.33,385.29 606.67,385.46 610.00,385.60 613.33,385.70 616.67,385.79 620.00,385.85 623.33,385.89 626.67,385.93 630.00,385.95 633.33,385.97 636.67,385.98 640.00,385.99 643.33,385.99 646.67,385.99 650.00,386.00 653.33,386.00 656.67,386.00 660.00,386.00 663.33,386.00 666.67,386.00 670.00,386.00 673.33,386.00 676.67,386.00 680.00,386.00 683.33,386.00 686.67,386.00 690.00,386.00 693.33,386.00 696.67,386.00 700.00,386.00 703.33,386.00 706.67,386.00 710.00,386.00 713.33,386.00 716.67,386.00 720.00,386.00 723.33,386.00 726.67,386.00 730.00,386.00 733.33,386.00 736.67,386.00 740.00,386.00 743.33,386.00 746.67,386.00 750.00,386.00 753.33,386.00 756.67,386 760.00,386 763.33,386 766.67,386 770.00,386 773.33,386 776.67,386 780.00,386 783.33,386 786.67,386 790.00,386" fill="none" stroke="#5b6770" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <line x1="70" y1="386" x2="790" y2="386" stroke="#aaa" stroke-width="1"/>
> <line x1="138.74" y1="386" x2="138.74" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="138.74" y="404" text-anchor="middle" fill="#777" font-size="11">-1σ</text>
> <line x1="306.63" y1="386" x2="306.63" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="306.63" y="404" text-anchor="middle" fill="#777" font-size="11">1σ</text>
> <line x1="390.58" y1="386" x2="390.58" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="390.58" y="404" text-anchor="middle" fill="#777" font-size="11">2σ</text>
> <line x1="474.52" y1="386" x2="474.52" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="474.52" y="404" text-anchor="middle" fill="#777" font-size="11">3σ</text>
> <line x1="558.47" y1="386" x2="558.47" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="558.47" y="404" text-anchor="middle" fill="#777" font-size="11">4σ</text>
> <line x1="642.41" y1="386" x2="642.41" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="642.41" y="404" text-anchor="middle" fill="#777" font-size="11">5σ</text>
> <line x1="726.36" y1="386" x2="726.36" y2="391" stroke="#aaa" stroke-width="1"/>
> <text x="726.36" y="404" text-anchor="middle" fill="#777" font-size="11">6σ</text>
> <text x="222.69" y="404" text-anchor="middle" fill="#999" font-size="11">average</text>
> <text x="222.69" y="418" text-anchor="middle" fill="#bbb" font-size="10">0.30</text>
> <line x1="508.06" y1="386" x2="508.06" y2="50" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="508.06" cy="386" r="3" fill="#3a86c8"/>
> <text x="508.06" y="46" text-anchor="middle" fill="#3a86c8" font-size="11">Mbappe 3.4σ</text>
> <line x1="521.84" y1="386" x2="521.84" y2="65" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="521.84" cy="386" r="3" fill="#3a86c8"/>
> <text x="521.84" y="61" text-anchor="middle" fill="#3a86c8" font-size="11">Lewandowski 3.6σ</text>
> <line x1="567.76" y1="386" x2="567.76" y2="80" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="567.76" cy="386" r="3" fill="#3a86c8"/>
> <text x="567.76" y="76" text-anchor="middle" fill="#3a86c8" font-size="11">Haaland 4.1σ</text>
> <line x1="590.71" y1="386" x2="590.71" y2="50" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="590.71" cy="386" r="3" fill="#3a86c8"/>
> <text x="590.71" y="46" text-anchor="middle" fill="#3a86c8" font-size="11">Ronaldo 4.4σ</text>
> <line x1="751.43" y1="386" x2="751.43" y2="50" stroke="#ff2e88" stroke-width="1.80"/>
> <circle cx="751.43" cy="386" r="6" fill="#ff2e88"/>
> <text x="751.43" y="46" text-anchor="end" fill="#ff2e88" font-size="13">Messi 6.3σ</text>
> </svg>

---

## Heatmap

`GeomTile` draws a colored rectangle per (x, y). Cell size = minimum
spacing along each axis. When `aesFill` maps to a numeric column the
cells are shaded along a 6-stop cold→hot gradient (blue / bright blue
/ bright cyan / bright green / bright yellow / bright red); without
`aesFill` every cell takes the layer's default color.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

coords =
    [ (fromIntegral x, fromIntegral y,
       sin (fromIntegral x / 2) + cos (fromIntegral y / 2))
    | x <- [0..4 :: Int], y <- [0..4 :: Int]
    ]
df =
    fromColumns
        [ ("x", ColNum [x | (x, _, _) <- coords])
        , ("y", ColNum [y | (_, y, _) <- coords])
        , ("z", ColNum [z | (_, _, z) <- coords])
        ]
layer =
    (defLayer GeomTile)
        { layerMapping =
            emptyMapping
                { aesX    = Just (ColumnRef "x")
                , aesY    = Just (ColumnRef "y")
                , aesFill = Just (ColumnRef "z")
                }
        , layerStat = StatIdentity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Heatmap"
        , chartSize = SizeChars 48 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 480 256" width="480" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="460" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="104.09" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="257.50" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="410.91" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="208.33" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="134.77" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="61.21" text-anchor="end" fill="#7f8c8d" font-size="11">4.0</text>
> <rect x="65.74" y="186.27" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="65.74" y="149.49" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="65.74" y="112.71" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="65.74" y="75.93" width="76.70" height="36.78" fill="#3498db"/>
> <rect x="65.74" y="39.15" width="76.70" height="36.78" fill="#2980b9"/>
> <rect x="142.44" y="186.27" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="142.44" y="149.49" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="142.44" y="112.71" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="142.44" y="75.93" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="142.44" y="39.15" width="76.70" height="36.78" fill="#3498db"/>
> <rect x="219.15" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="219.15" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="219.15" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="219.15" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="219.15" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="295.85" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="295.85" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="295.85" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="295.85" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="295.85" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="372.56" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="372.56" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="372.56" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="372.56" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="372.56" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <text x="257.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Heatmap</text>
> </svg>

---

## Annotated heatmap

Two layers: `GeomTile` underneath, `GeomText` on top reading
`aesLabel` for the cell label.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

coords =
    [ (fromIntegral x, fromIntegral y,
       sin (fromIntegral x / 2) + cos (fromIntegral y / 2))
    | x <- [0..4 :: Int], y <- [0..4 :: Int]
    ]
labels = [Text.pack (show (round (z * 10) :: Int)) | (_, _, z) <- coords]
df =
    fromColumns
        [ ("x",     ColNum [x | (x, _, _) <- coords])
        , ("y",     ColNum [y | (_, y, _) <- coords])
        , ("z",     ColNum [z | (_, _, z) <- coords])
        , ("label", ColCat labels)
        ]
tile =
    (defLayer GeomTile)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                , aesFill = Just (ColumnRef "z")
                }
        , layerStat = StatIdentity
        }
label =
    (defLayer GeomText)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                , aesLabel = Just (ColumnRef "label")
                }
        , layerStat = StatIdentity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [tile, label]
        , chartTitle = Just "Annotated heatmap"
        , chartSize = SizeChars 48 16
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 480 256" width="480" height="256" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="228.20" x2="460" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="228.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="104.09" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="257.50" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="410.91" y="243.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="49" y="208.33" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="134.77" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="61.21" text-anchor="end" fill="#7f8c8d" font-size="11">4.0</text>
> <rect x="65.74" y="186.27" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="65.74" y="149.49" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="65.74" y="112.71" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="65.74" y="75.93" width="76.70" height="36.78" fill="#3498db"/>
> <rect x="65.74" y="39.15" width="76.70" height="36.78" fill="#2980b9"/>
> <rect x="142.44" y="186.27" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="142.44" y="149.49" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="142.44" y="112.71" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="142.44" y="75.93" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="142.44" y="39.15" width="76.70" height="36.78" fill="#3498db"/>
> <rect x="219.15" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="219.15" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="219.15" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="219.15" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="219.15" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="295.85" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="295.85" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="295.85" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="295.85" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="295.85" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <rect x="372.56" y="186.27" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="372.56" y="149.49" width="76.70" height="36.78" fill="#e74c3c"/>
> <rect x="372.56" y="112.71" width="76.70" height="36.78" fill="#f1c40f"/>
> <rect x="372.56" y="75.93" width="76.70" height="36.78" fill="#2ecc71"/>
> <rect x="372.56" y="39.15" width="76.70" height="36.78" fill="#1abc9c"/>
> <text x="104.09" y="204.66" text-anchor="middle" fill="#9b59b6" font-size="11">10</text>
> <text x="104.09" y="167.88" text-anchor="middle" fill="#9b59b6" font-size="11">9</text>
> <text x="104.09" y="131.10" text-anchor="middle" fill="#9b59b6" font-size="11">5</text>
> <text x="104.09" y="94.32" text-anchor="middle" fill="#9b59b6" font-size="11">1</text>
> <text x="104.09" y="57.54" text-anchor="middle" fill="#9b59b6" font-size="11">-4</text>
> <text x="180.80" y="204.66" text-anchor="middle" fill="#9b59b6" font-size="11">15</text>
> <text x="180.80" y="167.88" text-anchor="middle" fill="#9b59b6" font-size="11">14</text>
> <text x="180.80" y="131.10" text-anchor="middle" fill="#9b59b6" font-size="11">10</text>
> <text x="180.80" y="94.32" text-anchor="middle" fill="#9b59b6" font-size="11">6</text>
> <text x="180.80" y="57.54" text-anchor="middle" fill="#9b59b6" font-size="11">1</text>
> <text x="257.50" y="204.66" text-anchor="middle" fill="#9b59b6" font-size="11">18</text>
> <text x="257.50" y="167.88" text-anchor="middle" fill="#9b59b6" font-size="11">17</text>
> <text x="257.50" y="131.10" text-anchor="middle" fill="#9b59b6" font-size="11">14</text>
> <text x="257.50" y="94.32" text-anchor="middle" fill="#9b59b6" font-size="11">9</text>
> <text x="257.50" y="57.54" text-anchor="middle" fill="#9b59b6" font-size="11">4</text>
> <text x="334.20" y="204.66" text-anchor="middle" fill="#9b59b6" font-size="11">20</text>
> <text x="334.20" y="167.88" text-anchor="middle" fill="#9b59b6" font-size="11">19</text>
> <text x="334.20" y="131.10" text-anchor="middle" fill="#9b59b6" font-size="11">15</text>
> <text x="334.20" y="94.32" text-anchor="middle" fill="#9b59b6" font-size="11">11</text>
> <text x="334.20" y="57.54" text-anchor="middle" fill="#9b59b6" font-size="11">6</text>
> <text x="410.91" y="204.66" text-anchor="middle" fill="#9b59b6" font-size="11">19</text>
> <text x="410.91" y="167.88" text-anchor="middle" fill="#9b59b6" font-size="11">18</text>
> <text x="410.91" y="131.10" text-anchor="middle" fill="#9b59b6" font-size="11">14</text>
> <text x="410.91" y="94.32" text-anchor="middle" fill="#9b59b6" font-size="11">10</text>
> <text x="410.91" y="57.54" text-anchor="middle" fill="#9b59b6" font-size="11">5</text>
> <text x="257.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Annotated heatmap</text>
> </svg>

---

## Funnel

A funnel is a horizontal bar chart with monotonically decreasing
values.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = ["Visited", "Signed up", "Confirmed", "Active", "Paid"]
ys = [1000, 720, 480, 220, 120] :: [Double]
df = fromColumns [("stage", ColCat xs), ("count", ColNum ys)]
layer =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "stage")
                , aesY = Just (ColumnRef "count")
                }
        , layerStat = StatIdentity
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartCoord = CoordFlip
        , chartTitle = Just "Sales funnel"
        , chartSize = SizeChars 60 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 224" width="600" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="71.40" y1="196.20" x2="480" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="71.40" y1="34" x2="71.40" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="89.97" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="275.70" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">500.0</text>
> <text x="461.43" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1000.0</text>
> <text x="65.40" y="57.33" text-anchor="end" fill="#7f8c8d" font-size="11">Visited</text>
> <text x="65.40" y="88.05" text-anchor="end" fill="#7f8c8d" font-size="11">Signed up</text>
> <text x="65.40" y="118.77" text-anchor="end" fill="#7f8c8d" font-size="11">Confirmed</text>
> <text x="65.40" y="149.49" text-anchor="end" fill="#7f8c8d" font-size="11">Active</text>
> <text x="65.40" y="180.21" text-anchor="end" fill="#7f8c8d" font-size="11">Paid</text>
> <rect x="89.97" y="41.37" width="371.45" height="24.58" fill="#3498db"/>
> <rect x="89.97" y="72.09" width="267.45" height="24.58" fill="#3498db"/>
> <rect x="89.97" y="102.81" width="178.30" height="24.58" fill="#3498db"/>
> <rect x="89.97" y="133.53" width="81.72" height="24.58" fill="#3498db"/>
> <rect x="89.97" y="164.25" width="44.57" height="24.58" fill="#3498db"/>
> <text x="275.70" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Sales funnel</text>
> <rect x="495" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Waterfall

Each bar sits on a manually-supplied `__ybase`. Negative deltas
(yend < ystart) render correctly — the rect uses the absolute span.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = ["Start", "Q1", "Q2", "Q3", "Refund", "Net"]
ystart = [0, 100, 130, 180, 175, 0]     :: [Double]
yend = [100, 130, 180, 175, 195, 195] :: [Double]
df =
    fromColumns
        [ ("x",       ColCat xs)
        , ("y",       ColNum yend)
        , ("__ybase", ColNum ystart)
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
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartTitle = Just "Waterfall"
        , chartSize = SizeChars 60 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 224" width="600" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="480" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="100.96" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Start</text>
> <text x="167.58" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q1</text>
> <text x="234.19" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q2</text>
> <text x="300.81" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Q3</text>
> <text x="367.42" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Refund</text>
> <text x="434.04" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Net</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="116.88" text-anchor="end" fill="#7f8c8d" font-size="11">100.0</text>
> <text x="49" y="41.26" text-anchor="end" fill="#7f8c8d" font-size="11">200.0</text>
> <rect x="74.32" y="113.21" width="53.29" height="75.62" fill="#3498db"/>
> <rect x="140.93" y="90.52" width="53.29" height="22.69" fill="#3498db"/>
> <rect x="207.55" y="52.72" width="53.29" height="37.81" fill="#3498db"/>
> <rect x="274.16" y="52.72" width="53.29" height="3.78" fill="#3498db"/>
> <rect x="340.78" y="41.37" width="53.29" height="15.12" fill="#3498db"/>
> <rect x="407.39" y="41.37" width="53.29" height="147.45" fill="#3498db"/>
> <text x="267.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Waterfall</text>
> <rect x="495" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Polar charts

`CoordPolar aes startAngle dir` projects the chosen aesthetic onto an
angle (0..2π) and the other onto a radius. Useful for cyclic data and
rose plots.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

nSteps = 32 :: Int
pts =
    [ (theta, abs (sin (2 * theta)))
    | i <- [0..nSteps]
    , let theta = (fromIntegral i / fromIntegral nSteps) * 2 * pi
    ]
df =
    fromColumns
        [ ("theta", ColNum [t | (t, _) <- pts])
        , ("r",     ColNum [r | (_, r) <- pts])
        ]
layer =
    (defLayer GeomLine)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "theta")
                , aesY = Just (ColumnRef "r")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartCoord = CoordPolar ThetaX 0 PolarCCW
        , chartTitle = Just "r = |sin(2 theta)|"
        , chartSize = SizeChars 40 20
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 320" width="400" height="320" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <polyline points="172.61,163.10 172.59,163.60 172.52,164.10 172.39,164.58 172.22,165.06 172.01,165.51 171.75,165.94 171.45,166.34 171.12,166.72 170.74,167.05 170.34,167.35 169.91,167.61 169.46,167.82 168.98,167.99 168.50,168.12 168.00,168.19 167.50,168.21 167.00,168.19 166.50,168.12 166.02,167.99 165.54,167.82 165.09,167.61 164.66,167.35 164.26,167.05 163.88,166.72 163.55,166.34 163.25,165.94 162.99,165.51 162.78,165.06 162.61,164.58 162.48,164.10 162.41,163.60 162.39,163.10 162.41,162.60 162.48,162.10 162.61,161.62 162.78,161.14 162.99,160.69 163.25,160.26 163.55,159.86 163.88,159.48 164.26,159.15 164.66,158.85 165.09,158.59 165.54,158.38 166.02,158.21 166.50,158.08 167.00,158.01 167.50,157.99 168.00,158.01 168.50,158.08 168.98,158.21 169.46,158.38 169.91,158.59 170.34,158.85 170.74,159.15 171.12,159.48 171.45,159.86 171.75,160.26 172.01,160.69 172.22,161.14 172.39,161.62 172.52,162.10 172.59,162.60 172.61,163.10" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="223.75,163.10 223.48,168.61 222.67,174.07 221.33,179.43 219.47,184.63 217.11,189.62 214.27,194.35 210.98,198.78 207.27,202.87 203.18,206.58 198.75,209.87 194.02,212.71 189.03,215.07 183.83,216.93 178.47,218.27 173.01,219.08 167.50,219.35 161.99,219.08 156.53,218.27 151.17,216.93 145.97,215.07 140.98,212.71 136.25,209.87 131.82,206.58 127.73,202.87 124.02,198.78 120.73,194.35 117.89,189.62 115.53,184.63 113.67,179.43 112.33,174.07 111.52,168.61 111.25,163.10 111.52,157.59 112.33,152.13 113.67,146.77 115.53,141.57 117.89,136.58 120.73,131.85 124.02,127.42 127.73,123.33 131.82,119.62 136.25,116.33 140.98,113.49 145.97,111.13 151.17,109.27 156.53,107.93 161.99,107.12 167.50,106.85 173.01,107.12 178.47,107.93 183.83,109.27 189.03,111.13 194.02,113.49 198.75,116.33 203.18,119.62 207.27,123.33 210.98,127.42 214.27,131.85 217.11,136.58 219.47,141.57 221.33,146.77 222.67,152.13 223.48,157.59 223.75,163.10" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="274.89,163.10 274.37,173.63 272.82,184.05 270.26,194.27 266.71,204.19 262.21,213.72 256.79,222.76 250.51,231.23 243.43,239.03 235.63,246.11 227.16,252.39 218.12,257.81 208.59,262.31 198.67,265.86 188.45,268.42 178.03,269.97 167.50,270.49 156.97,269.97 146.55,268.42 136.33,265.86 126.41,262.31 116.88,257.81 107.84,252.39 99.37,246.11 91.57,239.03 84.49,231.23 78.21,222.76 72.79,213.72 68.29,204.19 64.74,194.27 62.18,184.05 60.63,173.63 60.11,163.10 60.63,152.57 62.18,142.15 64.74,131.93 68.29,122.01 72.79,112.48 78.21,103.44 84.49,94.97 91.57,87.17 99.37,80.09 107.84,73.81 116.88,68.39 126.41,63.89 136.33,60.34 146.55,57.78 156.97,56.23 167.50,55.71 178.03,56.23 188.45,57.78 198.67,60.34 208.59,63.89 218.12,68.39 227.16,73.81 235.63,80.09 243.43,87.17 250.51,94.97 256.79,103.44 262.21,112.48 266.71,122.01 270.26,131.93 272.82,142.15 274.37,152.57 274.89,163.10" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,163.10 275.44,131.41" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,163.10 110.34,66.20" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,163.10 87.55,242.25" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="167.50,163.10 263.82,221.23" fill="none" stroke="#bdc3c7" stroke-width="0.50" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="280,163.10 279.46,174.13 277.84,185.05 275.16,195.76 271.44,206.15 266.72,216.13 261.04,225.60 254.46,234.47 247.05,242.65 238.87,250.06 230.00,256.64 220.53,262.32 210.55,267.04 200.16,270.76 189.45,273.44 178.53,275.06 167.50,275.60 156.47,275.06 145.55,273.44 134.84,270.76 124.45,267.04 114.47,262.32 105.00,256.64 96.13,250.06 87.95,242.65 80.54,234.47 73.96,225.60 68.28,216.13 63.56,206.15 59.84,195.76 57.16,185.05 55.54,174.13 55,163.10 55.54,152.07 57.16,141.15 59.84,130.44 63.56,120.05 68.28,110.07 73.96,100.60 80.54,91.73 87.95,83.55 96.13,76.14 105.00,69.56 114.47,63.88 124.45,59.16 134.84,55.44 145.55,52.76 156.47,51.14 167.50,50.60 178.53,51.14 189.45,52.76 200.16,55.44 210.55,59.16 220.53,63.88 230.00,69.56 238.87,76.14 247.05,83.55 254.46,91.73 261.04,100.60 266.72,110.07 271.44,120.05 275.16,130.44 277.84,141.15 279.46,152.07 280,163.10" fill="none" stroke="#ecf0f1" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="285.04" y="132.25" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="105.26" y="61.26" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="80.45" y="252.95" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="272.38" y="230.07" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="175.61" y="161.10" text-anchor="start" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="226.75" y="161.10" text-anchor="start" fill="#7f8c8d" font-size="11">0.5</text>
> <text x="277.89" y="161.10" text-anchor="start" fill="#7f8c8d" font-size="11">1.0</text>
> <polyline points="172.41,161.66 207.07,143.29 229.49,116.70 235.37,90.20 225.56,72.76 205.62,71.08 183.96,87.44 169.08,118.88 166.77,158.04 153.53,121.11 130.39,95.14 105.00,85.55 86.34,92.78 81.84,112.28 94.95,136.04 123.95,155.24 162.39,163.10 123.95,170.96 94.95,190.16 81.84,213.92 86.34,233.42 105.00,240.65 130.39,231.06 153.53,205.09 166.77,168.16 169.08,207.32 183.96,238.76 205.62,255.12 225.56,253.44 235.37,236.00 229.49,209.50 207.07,182.91 172.41,164.54" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="167.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">r = |sin(2 theta)|</text>
> <rect x="295" y="39" width="12" height="12" fill="#3498db"/>
> <text x="311" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Faceted charts

Split a chart into a grid of panels by a categorical column.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

df =
    fromColumns
        [ ("x",      ColNum  [0,1,2,3, 0,1,2,3, 0,1,2,3])
        , ("y",      ColNum  [1,4,9,16, 0,2,4,6, 5,4,3,2])
        , ("series", ColCat  (replicate 4 "A" <> replicate 4 "B" <> replicate 4 "C"))
        ]
layer =
    (defLayer GeomPoint)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartFacet = FacetWrap (ColumnRef "series") (Just 3) Nothing ScalesFixed
        , chartTitle = Just "Faceted by series"
        , chartSize = SizeChars 70 18
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 700 288" width="700" height="288" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="60" y1="246.20" x2="225" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="60" y1="66" x2="60" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="67.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="117.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="167.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="217.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">3.0</text>
> <text x="54" y="241.68" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="54" y="190.48" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="54" y="139.29" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="54" y="88.10" text-anchor="end" fill="#7f8c8d" font-size="11">15.0</text>
> <circle cx="67.50" cy="227.77" r="3" fill="#3498db"/>
> <circle cx="117.50" cy="197.05" r="3" fill="#3498db"/>
> <circle cx="167.50" cy="145.86" r="3" fill="#3498db"/>
> <circle cx="217.50" cy="74.19" r="3" fill="#3498db"/>
> <text x="142.50" y="62" text-anchor="middle" fill="#555555" font-size="11">A</text>
> <line x1="235" y1="246.20" x2="400" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="235" y1="66" x2="235" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="242.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="292.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="342.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="392.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">3.0</text>
> <text x="229" y="241.68" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="229" y="190.48" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="229" y="139.29" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="229" y="88.10" text-anchor="end" fill="#7f8c8d" font-size="11">15.0</text>
> <circle cx="242.50" cy="238.01" r="3" fill="#3498db"/>
> <circle cx="292.50" cy="217.53" r="3" fill="#3498db"/>
> <circle cx="342.50" cy="197.05" r="3" fill="#3498db"/>
> <circle cx="392.50" cy="176.58" r="3" fill="#3498db"/>
> <text x="317.50" y="62" text-anchor="middle" fill="#555555" font-size="11">B</text>
> <line x1="410" y1="246.20" x2="575" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="410" y1="66" x2="410" y2="246.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="417.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="467.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="517.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="567.50" y="261.20" text-anchor="middle" fill="#7f8c8d" font-size="11">3.0</text>
> <text x="404" y="241.68" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="404" y="190.48" text-anchor="end" fill="#7f8c8d" font-size="11">5.0</text>
> <text x="404" y="139.29" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="404" y="88.10" text-anchor="end" fill="#7f8c8d" font-size="11">15.0</text>
> <circle cx="417.50" cy="186.82" r="3" fill="#3498db"/>
> <circle cx="467.50" cy="197.05" r="3" fill="#3498db"/>
> <circle cx="517.50" cy="207.29" r="3" fill="#3498db"/>
> <circle cx="567.50" cy="217.53" r="3" fill="#3498db"/>
> <text x="492.50" y="62" text-anchor="middle" fill="#555555" font-size="11">C</text>
> <text x="317.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Faceted by series</text>
> <rect x="595" y="39" width="12" height="12" fill="#3498db"/>
> <text x="611" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Log-Y scatter

`SLog Base10` on `scaleY` for a log-scale Y axis. The `defScaleOpts`
default produces "nice" integer-power breaks (1, 10, 100, …).

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [1..6] :: [Double]
ys = [3, 30, 80, 200, 700, 2100] :: [Double]
df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
layer =
    (defLayer GeomPoint)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [layer]
        , chartScales = defScales{ scaleY = SLog Base10 defScaleOpts }
        , chartTitle = Just "Log Y"
        , chartSize = SizeChars 50 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 224" width="500" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="380" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="128.86" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="247.05" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="365.23" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="165.39" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="113.57" text-anchor="end" fill="#7f8c8d" font-size="11">100.0</text>
> <text x="49" y="61.74" text-anchor="end" fill="#7f8c8d" font-size="11">1000.0</text>
> <circle cx="69.77" cy="188.83" r="3" fill="#3498db"/>
> <circle cx="128.86" cy="137.00" r="3" fill="#3498db"/>
> <circle cx="187.95" cy="114.92" r="3" fill="#3498db"/>
> <circle cx="247.05" cy="94.30" r="3" fill="#3498db"/>
> <circle cx="306.14" cy="66.10" r="3" fill="#3498db"/>
> <circle cx="365.23" cy="41.37" r="3" fill="#3498db"/>
> <text x="217.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Log Y</text>
> <rect x="395" y="39" width="12" height="12" fill="#3498db"/>
> <text x="411" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> </svg>

---

## Layered charts

Granite charts can carry any number of layers — each one a different
geom on the same axes. Layers share the chart-level data frame by
default, or supply their own via `layerData`. Aesthetic defaults
(`layerAesDef`) override the layer's color, point size, line width,
and fill opacity without needing a custom theme.

### Bar + trend line

The classic "report" chart: vertical bars for the raw values plus a
line tracking a smoothed estimate (moving average, target, etc.).
Both layers read from the same data frame.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun"]
sales = [12, 18, 15, 24, 20, 28] :: [Double]
movavg = [12, 15, 16.5, 19.5, 22, 24] :: [Double]
df =
    fromColumns
        [ ("month", ColCat months)
        , ("sales", ColNum sales)
        , ("avg",   ColNum movavg)
        ]
bars =
    (defLayer GeomBar)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "month")
                , aesY = Just (ColumnRef "sales")
                }
        , layerStat = StatIdentity
        }
trend =
    (defLayer GeomLine)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "month")
                , aesY = Just (ColumnRef "avg")
                }
        , layerAesDef =
            emptyAesDefaults
                { defColor = Just (NamedColor BrightRed)
                , defLineWidth = Just 2.5
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [bars, trend]
        , chartTitle = Just "Monthly sales + trend"
        , chartSize = SizeChars 60 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 224" width="600" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="480" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="100.96" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Jan</text>
> <text x="167.58" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Feb</text>
> <text x="234.19" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Mar</text>
> <text x="300.81" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Apr</text>
> <text x="367.42" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">May</text>
> <text x="434.04" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">Jun</text>
> <text x="49" y="192.49" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="139.83" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="87.17" text-anchor="end" fill="#7f8c8d" font-size="11">20.0</text>
> <rect x="74.32" y="125.63" width="53.29" height="63.19" fill="#3498db"/>
> <rect x="140.93" y="94.04" width="53.29" height="94.79" fill="#3498db"/>
> <rect x="207.55" y="109.83" width="53.29" height="78.99" fill="#3498db"/>
> <rect x="274.16" y="62.44" width="53.29" height="126.39" fill="#3498db"/>
> <rect x="340.78" y="83.50" width="53.29" height="105.32" fill="#3498db"/>
> <rect x="407.39" y="41.37" width="53.29" height="147.45" fill="#3498db"/>
> <polyline points="100.96,125.63 167.58,109.83 234.19,101.93 300.81,86.14 367.42,72.97 434.04,62.44" fill="none" stroke="#e74c3c" stroke-width="2.50" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="267.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Monthly sales + trend</text>
> <rect x="495" y="39" width="12" height="12" fill="#3498db"/>
> <text x="511" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="495" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="511" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

### Scatter + radius circle

A scatter of geographic points (longitude / latitude) with a single
semi-transparent disc marking a "radius around a point of interest".
The two layers use **different** data frames — the radius layer
supplies its own one-row frame via `layerData`, and `defSize` /
`defAlpha` on `layerAesDef` make the POI render as a fat translucent
puck instead of the default 3-pixel dot.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

pts =
    [ (0.2, 0.5), (0.8, 1.2), (-0.5, 0.3), (1.1, -0.2)
    , (1.8, 0.7), (-0.3, -0.6), (2.2, 1.5), (1.5, -1.1)
    , (0.1, 2.1), (2.5, 0.3), (-1.0, 1.0), (0.7, -0.4)
    ] :: [(Double, Double)]
points =
    fromColumns
        [ ("lon", ColNum [x | (x, _) <- pts])
        , ("lat", ColNum [y | (_, y) <- pts])
        ]
poi =
    fromColumns
        [ ("lon", ColNum [0.5])
        , ("lat", ColNum [0.5])
        ]
radius =
    (defLayer GeomPoint)
        { layerData = Just poi
        , layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "lon")
                , aesY = Just (ColumnRef "lat")
                }
        , layerAesDef =
            emptyAesDefaults
                { defColor = Just (NamedColor BrightCyan)
                , defSize  = Just 60      -- screen-px radius
                , defAlpha = Just 0.25
                }
        }
scatter =
    (defLayer GeomPoint)
        { layerData = Just points
        , layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "lon")
                , aesY = Just (ColumnRef "lat")
                }
        }
chart =
    emptyChart
        { chartLayers = [radius, scatter]   -- radius first → points draw on top
        , chartTitle = Just "Points near POI"
        , chartSize = SizeChars 50 18
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 288" width="500" height="288" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="260.20" x2="380" y2="260.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="260.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="69.77" y="275.20" text-anchor="middle" fill="#7f8c8d" font-size="11">-1.0</text>
> <text x="154.19" y="275.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="238.60" y="275.20" text-anchor="middle" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="323.02" y="275.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="49" y="247.16" text-anchor="end" fill="#7f8c8d" font-size="11">-1.0</text>
> <text x="49" y="182.90" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="118.64" text-anchor="end" fill="#7f8c8d" font-size="11">1.0</text>
> <text x="49" y="54.37" text-anchor="end" fill="#7f8c8d" font-size="11">2.0</text>
> <circle cx="196.40" cy="147.10" r="60" fill="#1abc9c" fill-opacity="0.25"/>
> <circle cx="171.07" cy="147.10" r="3" fill="#9b59b6"/>
> <circle cx="221.72" cy="102.12" r="3" fill="#9b59b6"/>
> <circle cx="111.98" cy="159.95" r="3" fill="#9b59b6"/>
> <circle cx="247.05" cy="192.08" r="3" fill="#9b59b6"/>
> <circle cx="306.14" cy="134.25" r="3" fill="#9b59b6"/>
> <circle cx="128.86" cy="217.79" r="3" fill="#9b59b6"/>
> <circle cx="339.90" cy="82.84" r="3" fill="#9b59b6"/>
> <circle cx="280.81" cy="249.92" r="3" fill="#9b59b6"/>
> <circle cx="162.63" cy="44.28" r="3" fill="#9b59b6"/>
> <circle cx="365.23" cy="159.95" r="3" fill="#9b59b6"/>
> <circle cx="69.77" cy="114.97" r="3" fill="#9b59b6"/>
> <circle cx="213.28" cy="204.94" r="3" fill="#9b59b6"/>
> <text x="217.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Points near POI</text>
> <rect x="395" y="39" width="12" height="12" fill="#3498db"/>
> <text x="411" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="395" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="411" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

> The radius is in screen pixels, not data units — granite doesn't
> yet have a "data-unit circle" primitive. For most overlays that's
> fine; if you need a circle whose radius scales with the axes you'd
> need to compute the pixel size from the projector yourself.

### Scatter + best-fit line

Scatter shows the raw points; a `GeomLine` layer with
`layerStat = StatSmooth SmoothLm` runs an ordinary-least-squares
regression and draws the fitted line on top.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
noise = [0.3, -0.5, 0.7, -0.2, 0.4, -0.6, 0.1, 0.3, -0.4, 0.5]
ys = [2 * x + 1 + n | (x, n) <- zip xs noise]
df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
m =
    emptyMapping
        { aesX = Just (ColumnRef "x")
        , aesY = Just (ColumnRef "y")
        }
points = (defLayer GeomPoint){ layerMapping = m }
fit =
    (defLayer GeomLine)
        { layerMapping = m
        , layerStat = StatSmooth SmoothLm
        , layerAesDef =
            emptyAesDefaults
                { defColor = Just (NamedColor BrightRed)
                , defLineWidth = Just 2
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [points, fit]
        , chartTitle = Just "Scatter + OLS fit"
        , chartSize = SizeChars 50 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 224" width="500" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="380" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="69.77" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="135.43" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="201.09" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="266.74" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="332.40" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">8.0</text>
> <text x="49" y="120.96" text-anchor="end" fill="#7f8c8d" font-size="11">10.0</text>
> <text x="49" y="41.04" text-anchor="end" fill="#7f8c8d" font-size="11">20.0</text>
> <circle cx="69.77" cy="186.82" r="3" fill="#3498db"/>
> <circle cx="102.60" cy="177.23" r="3" fill="#3498db"/>
> <circle cx="135.43" cy="151.66" r="3" fill="#3498db"/>
> <circle cx="168.26" cy="142.87" r="3" fill="#3498db"/>
> <circle cx="201.09" cy="122.09" r="3" fill="#3498db"/>
> <circle cx="233.91" cy="114.10" r="3" fill="#3498db"/>
> <circle cx="266.74" cy="92.52" r="3" fill="#3498db"/>
> <circle cx="299.57" cy="74.94" r="3" fill="#3498db"/>
> <circle cx="332.40" cy="64.55" r="3" fill="#3498db"/>
> <circle cx="365.23" cy="41.37" r="3" fill="#3498db"/>
> <polyline points="69.77,188.83 102.60,172.82 135.43,156.82 168.26,140.82 201.09,124.82 233.91,108.81 266.74,92.81 299.57,76.81 332.40,60.80 365.23,44.80" fill="none" stroke="#e74c3c" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="217.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Scatter + OLS fit</text>
> <rect x="395" y="39" width="12" height="12" fill="#3498db"/>
> <text x="411" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="395" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="411" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

`SmoothLoess` and `SmoothMovingAvg` work the same way — just swap
the `SmoothMethod` argument.

### Line + confidence ribbon

A central estimate with a translucent ±band around it. The ribbon
layer reads `aesYmin` / `aesYmax`; the line reads `aesY`.

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Text.IO as Text.IO
import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

xs = [0, 0.5 .. 6.0] :: [Double]
ys = map sin xs
los = map (\x -> sin x - 0.3) xs
his = map (\x -> sin x + 0.3) xs
df =
    fromColumns
        [ ("x",  ColNum xs)
        , ("y",  ColNum ys)
        , ("lo", ColNum los)
        , ("hi", ColNum his)
        ]
ribbon =
    (defLayer GeomRibbon)
        { layerMapping =
            emptyMapping
                { aesX    = Just (ColumnRef "x")
                , aesYmin = Just (ColumnRef "lo")
                , aesYmax = Just (ColumnRef "hi")
                }
        , layerAesDef =
            emptyAesDefaults
                { defColor = Just (NamedColor BrightCyan)
                , defAlpha = Just 0.3
                }
        }
line =
    (defLayer GeomLine)
        { layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "x")
                , aesY = Just (ColumnRef "y")
                }
        , layerAesDef =
            emptyAesDefaults
                { defColor = Just (NamedColor BrightBlue)
                , defLineWidth = Just 2
                }
        }
chart =
    emptyChart
        { chartData = df
        , chartLayers = [ribbon, line]    -- ribbon first → line on top
        , chartTitle = Just "sin(x) +/- 0.3 band"
        , chartSize = SizeChars 56 14
        }

Text.IO.putStrLn (renderChartSvg chart)
```

> <!-- scripths:mime text/plain -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 224" width="560" height="224" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="196.20" x2="440" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="196.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="72.50" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="189.17" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">2.0</text>
> <text x="305.83" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">4.0</text>
> <text x="422.50" y="211.20" text-anchor="middle" fill="#7f8c8d" font-size="11">6.0</text>
> <text x="49" y="176.60" text-anchor="end" fill="#7f8c8d" font-size="11">-1.0</text>
> <text x="49" y="119.34" text-anchor="end" fill="#7f8c8d" font-size="11">0.0</text>
> <text x="49" y="62.07" text-anchor="end" fill="#7f8c8d" font-size="11">1.0</text>
> <polygon points="72.50,98.49 101.67,71.04 130.83,50.31 160.00,41.37 189.17,46.42 218.33,64.22 247.50,90.41 276.67,118.58 305.83,141.83 335.00,154.47 364.17,153.40 393.33,138.89 422.50,114.49 422.50,148.85 393.33,173.25 364.17,187.76 335.00,188.83 305.83,176.19 276.67,152.94 247.50,124.77 218.33,98.58 189.17,80.78 160.00,75.73 130.83,84.67 101.67,105.40 72.50,132.85" fill="#1abc9c" stroke="#1abc9c" stroke-width="1" fill-opacity="0.40"/>
> <polyline points="72.50,115.67 101.67,88.22 130.83,67.49 160.00,58.55 189.17,63.60 218.33,81.40 247.50,107.59 276.67,135.76 305.83,159.01 335.00,171.65 364.17,170.58 393.33,156.07 422.50,131.67" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <text x="247.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">sin(x) +/- 0.3 band</text>
> <rect x="455" y="39" width="12" height="12" fill="#3498db"/>
> <text x="471" y="49" text-anchor="start" fill="#7f8c8d" font-size="11">series 0</text>
> <rect x="455" y="56" width="12" height="12" fill="#9b59b6"/>
> <text x="471" y="66" text-anchor="start" fill="#7f8c8d" font-size="11">series 1</text>
> </svg>

---

## Known limitations

Recorded as Phase 9-ish follow-up work:

- **Color by aesthetic** — stacked / grouped bars and heatmaps render
  in a single color per layer until `aesColor` / `aesFill` is wired
  to a per-row palette index.
- **Size by aesthetic** — `aesSize` declared but bubble charts aren't
  wired.
- **Categorical scales** — `ColCat` X / Y falls back to integer
  indices for projection; tick labels show the index rather than the
  category name.
- **Time formatting** — `FormatDateTime` is a stub; time-series labels
  currently show raw epoch milliseconds.
