# Revision history for granite

## 0.7.1.1 -- 2026-05-25
* `GeomTile` honours a continuous `scaleFill` (`SColorContinuous`) via smooth RGB interpolation, and no longer emits a stray category legend entry.

## 0.7.1.0 -- 2026-05-25

* Numeric `aesAlpha` mappings are now honoured for points (per-point opacity); previously only the per-layer `defAlpha` was applied.

## 0.7.0.0 -- 2026-05-24

* `Color` is now a 24-bit RGB value, and categorical colour mappings are honoured for points.
* Scatter plots now respect point colours.


## 0.5.0.0 -- 2026-05-13

A major refactor introducing a declarative, Grammar-of-Graphics-style
IR shared between the terminal and SVG backends. Legacy chart
functions (`scatter`, `bars`, `pie`, …) remain unchanged and continue
to work; the new IR is additive.

* New module `Granite.Spec` exposing the chart IR: `Chart`, `Layer`,
  `Mapping`, `Geom`, `Stat`, `Position`, `Scale`, `Coord`, `Facet`,
  `Theme`, `Size`, `ColorSpec`, `Formatter`. All types are plain ADTs
  of basic Haskell types so a downstream user can derive `ToJSON` /
  `FromJSON` instances with the JSON library of their choice. Granite
  itself ships no JSON instances; the dependency footprint remains
  `base + text`.
* New module `Granite.Data.Frame` providing a column-oriented
  `DataFrame` (`ColNum`, `ColCat`, `ColTime`, `ColBool`).
* New module `Granite.Format` with a declarative `Formatter` enum
  (`FormatPrecision`, `FormatScientific`, `FormatPercent`,
  `FormatComma`, `FormatSI`, …). Replaces the function-typed
  `LabelFormatter` for IR users; legacy `Plot { xFormatter, yFormatter }`
  remains in place.
* New module `Granite.Scale` with linear, log (base 2/e/10), sqrt,
  reverse, and identity scales, plus Heckbert-style "nice" tick
  selection. (Talbot–Lin–Hanrahan "Extended" tick scoring is a future
  upgrade — see the source for the TODO.)
* New module `Granite.Render.Scene` with backend-agnostic primitive
  marks: `MCircle`, `MRect`, `MPolyline`, `MPath`, `MText`, `MArc`,
  `MGroup`. Both backends consume the same `Scene`.
* New module `Granite.Render.Terminal` with the unified terminal
  backend. The Braille canvas and AVL-backed `Array2D` were extracted
  from `Granite` and now power both the legacy chart functions and
  the new IR pipeline.
* New module `Granite.Render.Svg` with the unified SVG backend.
  `renderSceneResponsive` produces SVG using `viewBox` +
  `preserveAspectRatio` + `width="100%"` so charts embed responsively.
* New module `Granite.Render.Chrome` with axes, ticks, gridlines,
  title, and legend as primitive marks — chrome code is no longer
  duplicated between backends.
* New module `Granite.Render.Pipeline` exposing `chartToScene`,
  `renderChartTerminal`, and `renderChartSvg`. Phase 3 supports the
  cartesian path with identity stat and identity position. Polar
  coord, facets, stats, positions, ribbons / errorbars, and multi-
  chart figures are scheduled for later releases.
* New module `Granite.Chart` with thin builders (`scatterChart`,
  `lineChart`) that construct an IR `Chart` from the same input shapes
  the legacy chart functions accept.
* New module `Granite.Color` extracted from `Granite`; exports the
  ANSI palette, ANSI escape helpers, and hex mapping.
* New module `Granite.Internal.Util` with the shared numeric, list,
  text, and tick helpers that used to live (duplicated) in both
  `Granite` and `Granite.Svg`.
* `ColorSpec` adds `RGB` and `Hex` constructors for cross-renderer
  color fidelity, with terminal backends quantising to the nearest
  ANSI color.

## 0.4.0.0 -- 2026-02-27
* Add Svg support.

## 0.3.0.5 -- 2025-11-10
* Fix x-axis label alignment for continuous histograms.

## 0.3.0.4 -- 2025-09-26
* Fix issue with x-axis spacing when categories is greater than width.

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
