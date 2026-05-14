{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Spec
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Declarative Grammar-of-Graphics chart IR: data + layers + scales +
coord + facet + theme + size.
-}
module Granite.Spec (
    Chart (..),
    emptyChart,
    module Granite.Data.Frame,
    module Granite.Format,
    ColumnRef (..),
    Mapping (..),
    emptyMapping,
    AesDefaults (..),
    emptyAesDefaults,
    ColorSpec (..),
    Layer (..),
    defLayer,
    Geom (..),
    Stat (..),
    BinSpec (..),
    SmoothMethod (..),
    SummaryFun (..),
    Position (..),
    Scale (..),
    ScaleOpts (..),
    defScaleOpts,
    LogBase (..),
    BreaksSpec (..),
    Expand (..),
    Scales (..),
    defScales,
    Coord (..),
    PolarAes (..),
    PolarDir (..),
    Facet (..),
    FacetScales (..),
    Theme (..),
    defTheme,
    Size (..),
) where

import Data.Text (Text)
import Data.Word (Word8)

import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Format

newtype ColumnRef = ColumnRef Text
    deriving (Eq, Show, Read)

data Mapping = Mapping
    { aesX :: Maybe ColumnRef
    , aesY :: Maybe ColumnRef
    , aesColor :: Maybe ColumnRef
    , aesFill :: Maybe ColumnRef
    , aesSize :: Maybe ColumnRef
    , aesAlpha :: Maybe ColumnRef
    , aesGroup :: Maybe ColumnRef
    , aesYmin :: Maybe ColumnRef
    , aesYmax :: Maybe ColumnRef
    , aesLabel :: Maybe ColumnRef
    }
    deriving (Eq, Show, Read)

emptyMapping :: Mapping
emptyMapping =
    Mapping
        { aesX = Nothing
        , aesY = Nothing
        , aesColor = Nothing
        , aesFill = Nothing
        , aesSize = Nothing
        , aesAlpha = Nothing
        , aesGroup = Nothing
        , aesYmin = Nothing
        , aesYmax = Nothing
        , aesLabel = Nothing
        }

data AesDefaults = AesDefaults
    { defColor :: Maybe ColorSpec
    , defFill :: Maybe ColorSpec
    , defSize :: Maybe Double
    , defAlpha :: Maybe Double
    , defLineWidth :: Maybe Double
    , defLineDash :: Maybe [Double]
    }
    deriving (Eq, Show, Read)

emptyAesDefaults :: AesDefaults
emptyAesDefaults =
    AesDefaults
        { defColor = Nothing
        , defFill = Nothing
        , defSize = Nothing
        , defAlpha = Nothing
        , defLineWidth = Nothing
        , defLineDash = Nothing
        }

data ColorSpec
    = NamedColor Color
    | RGB Word8 Word8 Word8
    | Hex Text
    deriving (Eq, Show, Read)

data Layer = Layer
    { layerData :: Maybe DataFrame
    , layerMapping :: Mapping
    , layerGeom :: Geom
    , layerStat :: Stat
    , layerPosition :: Position
    , layerAesDef :: AesDefaults
    }
    deriving (Eq, Show, Read)

defLayer :: Geom -> Layer
defLayer g =
    Layer
        { layerData = Nothing
        , layerMapping = emptyMapping
        , layerGeom = g
        , layerStat = defaultStat g
        , layerPosition = PosIdentity
        , layerAesDef = emptyAesDefaults
        }

defaultStat :: Geom -> Stat
defaultStat g = case g of
    GeomBar -> StatCount
    GeomBoxplot -> StatBoxplot
    GeomDensity -> StatDensity
    GeomHistogram -> StatBin (BinByCount 30)
    _ -> StatIdentity

data Geom
    = GeomPoint
    | GeomLine
    | GeomBar
    | GeomCol
    | GeomRibbon
    | GeomErrorbar
    | GeomTile
    | GeomBoxplot
    | GeomDensity
    | GeomHistogram
    | GeomText
    | GeomArc
    deriving (Eq, Show, Read)

data Stat
    = StatIdentity
    | StatBin BinSpec
    | StatDensity
    | StatSmooth SmoothMethod
    | StatBoxplot
    | StatCount
    | StatSummary SummaryFun
    deriving (Eq, Show, Read)

data BinSpec
    = BinByCount Int
    | BinByWidth Double
    | BinByEdges [Double]
    deriving (Eq, Show, Read)

data SmoothMethod
    = SmoothLm
    | SmoothLoess Double
    | SmoothMovingAvg Int
    deriving (Eq, Show, Read)

data SummaryFun
    = SumMean
    | SumMedian
    | SumQuantile Double
    | SumSum
    deriving (Eq, Show, Read)

data Position
    = PosIdentity
    | PosStack
    | PosDodge Double
    | PosJitter Double Double
    | PosFill
    deriving (Eq, Show, Read)

data LogBase = Base2 | BaseE | Base10
    deriving (Eq, Show, Read)

data Expand = Expand
    { expandMult :: Double
    , expandAdd :: Double
    }
    deriving (Eq, Show, Read)

data BreaksSpec
    = BreaksNice
    | BreaksCount Int
    | BreaksAt [Double]
    deriving (Eq, Show, Read)

data ScaleOpts = ScaleOpts
    { scaleDomain :: Maybe (Double, Double)
    , scaleBreaks :: BreaksSpec
    , scaleLabels :: Formatter
    , scaleExpand :: Expand
    , scaleClip :: Bool
    }
    deriving (Eq, Show, Read)

defScaleOpts :: ScaleOpts
defScaleOpts =
    ScaleOpts
        { scaleDomain = Nothing
        , scaleBreaks = BreaksNice
        , scaleLabels = FormatDefault
        , scaleExpand = Expand 0.05 0
        , scaleClip = False
        }

data Scale
    = SLinear ScaleOpts
    | SLog LogBase ScaleOpts
    | SSqrt ScaleOpts
    | SReverse Scale
    | SIdentity
    | SDiscrete
    | SColorContinuous [ColorSpec]
    | SColorDiscrete [ColorSpec]
    deriving (Eq, Show, Read)

data Scales = Scales
    { scaleX :: Scale
    , scaleY :: Scale
    , scaleColor :: Maybe Scale
    , scaleFill :: Maybe Scale
    , scaleSize :: Maybe Scale
    }
    deriving (Eq, Show, Read)

defScales :: Scales
defScales =
    Scales
        { scaleX = SLinear defScaleOpts
        , scaleY = SLinear defScaleOpts
        , scaleColor = Nothing
        , scaleFill = Nothing
        , scaleSize = Nothing
        }

data PolarAes = ThetaX | ThetaY
    deriving (Eq, Show, Read)

data PolarDir = PolarCW | PolarCCW
    deriving (Eq, Show, Read)

data Coord
    = CoordCartesian
    | CoordFlip
    | CoordPolar PolarAes Double PolarDir
    deriving (Eq, Show, Read)

data FacetScales
    = ScalesFixed
    | ScalesFreeX
    | ScalesFreeY
    | ScalesFree
    deriving (Eq, Show, Read)

data Facet
    = FacetNull
    | FacetWrap ColumnRef (Maybe Int) (Maybe Int) FacetScales
    | FacetGrid [ColumnRef] [ColumnRef] FacetScales
    deriving (Eq, Show, Read)

data Theme = Theme
    { themePalette :: [ColorSpec]
    , themePieColors :: [ColorSpec]
    , themeAxisColor :: ColorSpec
    , themeGridColor :: ColorSpec
    , themeTextColor :: ColorSpec
    , themeBackground :: ColorSpec
    , themeFontSize :: Double
    , themeTitleSize :: Double
    , themeStrokeWidth :: Double
    , themePxPerChar :: Double
    , themePxPerLine :: Double
    }
    deriving (Eq, Show, Read)

defTheme :: Theme
defTheme =
    Theme
        { themePalette =
            [ NamedColor BrightBlue
            , NamedColor BrightMagenta
            , NamedColor BrightCyan
            , NamedColor BrightGreen
            , NamedColor BrightYellow
            , NamedColor BrightRed
            , NamedColor BrightWhite
            , NamedColor BrightBlack
            ]
        , themePieColors =
            [ NamedColor BrightRed
            , NamedColor BrightGreen
            , NamedColor BrightYellow
            , NamedColor BrightBlue
            , NamedColor BrightMagenta
            , NamedColor BrightCyan
            , NamedColor BrightWhite
            , NamedColor BrightBlack
            ]
        , themeAxisColor = Hex "#aaaaaa"
        , themeGridColor = Hex "#eeeeee"
        , themeTextColor = Hex "#555555"
        , themeBackground = Hex "#ffffff"
        , themeFontSize = 11
        , themeTitleSize = 14
        , themeStrokeWidth = 1
        , themePxPerChar = 10
        , themePxPerLine = 16
        }

data Size
    = SizeChars Int Int
    | SizePixels Int Int
    | SizeResponsive Double
    deriving (Eq, Show, Read)

data Chart = Chart
    { chartData :: DataFrame
    , chartLayers :: [Layer]
    , chartScales :: Scales
    , chartCoord :: Coord
    , chartFacet :: Facet
    , chartTheme :: Theme
    , chartTitle :: Maybe Text
    , chartSize :: Size
    }
    deriving (Eq, Show, Read)

emptyChart :: Chart
emptyChart =
    Chart
        { chartData = emptyFrame
        , chartLayers = []
        , chartScales = defScales
        , chartCoord = CoordCartesian
        , chartFacet = FacetNull
        , chartTheme = defTheme
        , chartTitle = Nothing
        , chartSize = SizeChars 60 20
        }
