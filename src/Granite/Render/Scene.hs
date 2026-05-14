
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Render.Scene
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Backend-agnostic IR. Coordinates are in logical pixels; the terminal
backend maps them to character cells via 'pxPerChar' / 'pxPerLine'
(Braille gives 2×4 sub-cell resolution).
-}
module Granite.Render.Scene (
    Point (..),
    Rect (..),
    Mark (..),
    Scene (..),
    Style (..),
    defaultStyle,
    TextStyle (..),
    defaultTextStyle,
    TextAnchor (..),
    pxPerChar,
    pxPerLine,
) where

import Data.Text (Text)
import Granite.Color (Color (..))

data Point = Point !Double !Double
    deriving (Eq, Show)

-- | Axis-aligned rectangle by top-left + size.
data Rect = Rect
    { rectX :: !Double
    , rectY :: !Double
    , rectW :: !Double
    , rectH :: !Double
    }
    deriving (Eq, Show)

-- | 'Nothing' on fill or stroke means "don't paint that side".
data Style = Style
    { styleFill :: !(Maybe Color)
    , styleStroke :: !(Maybe Color)
    , styleStrokeWidth :: !Double
    , styleFillOpacity :: !Double
    }
    deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style Nothing Nothing 1 1

data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
    deriving (Eq, Show)

data TextStyle = TextStyle
    { textFill :: !Color
    , textSize :: !Double
    , textAnchor :: !TextAnchor
    }
    deriving (Eq, Show)

defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle Default 11 AnchorStart

{- | Primitive drawable marks. 'MArc' takes (center, radius, a0, a1)
in radians CCW from +x. 'MPolygon' is a closed filled polyline.
'MAxisLine' is an axis-aligned 1-px line that the terminal backend
writes as box-drawing chars (@│@ / @─@ / @┼@) for crisp axes;
everything else is rasterised through Braille.
-}
data Mark
    = MCircle !Point !Double !Style
    | MRect !Rect !Style
    | MPolyline ![Point] !Style
    | MPolygon ![Point] !Style
    | MPath !Text !Style
    | MText !Point !Text !TextStyle
    | MArc !Point !Double !Double !Double !Style
    | MAxisLine !Point !Point !Style
    | MGroup ![Mark]
    deriving (Eq, Show)

data Scene = Scene
    { sceneWidth :: !Double
    , sceneHeight :: !Double
    , sceneMarks :: ![Mark]
    }
    deriving (Eq, Show)

pxPerChar :: Double
pxPerChar = 10

pxPerLine :: Double
pxPerLine = 16
