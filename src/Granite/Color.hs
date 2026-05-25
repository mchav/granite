{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Color
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Color primitives shared by the terminal and SVG backends.

'Color' is a 24-bit RGB value. The classic ANSI palette names ('Black',
'BrightBlue', 'Default', …) are provided as bidirectional pattern synonyms
that expand to fixed RGB triples, so they may be used both as expressions
and in patterns exactly as before.

The two backends deliberately diverge on how a colour is realised:

  * the SVG backend renders the RGB exactly via 'colorHex';
  * the terminal backend approximates via 'ansiCode', which maps an
    arbitrary colour to the nearest of the 16 ANSI slots.

Because 'Color' is now a plain RGB triple, its /derived/ 'Show' prints
@Color r g b@ rather than the alias name.
-}
module Granite.Color (
    Color (
        ..,
        Black,
        Red,
        Green,
        Yellow,
        Blue,
        Magenta,
        Cyan,
        White,
        BrightBlack,
        BrightRed,
        BrightGreen,
        BrightYellow,
        BrightBlue,
        BrightMagenta,
        BrightCyan,
        BrightWhite,
        Default
    ),
    parseHex,
    ansiCode,
    ansiOn,
    ansiOff,
    paint,
    colorHex,
    paletteColors,
    pieColors,
) where

import Data.Char (digitToInt, isHexDigit)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Numeric (showHex)

-- | A 24-bit RGB colour.
data Color = Color !Word8 !Word8 !Word8
    deriving (Eq, Show, Read)

-- The classic ANSI palette, as bidirectional pattern-synonym aliases over
-- fixed RGB triples (the historical 'colorHex' values).

pattern Black :: Color
pattern Black = Color 0x2c 0x3e 0x50

pattern Red :: Color
pattern Red = Color 0xc0 0x39 0x2b

pattern Green :: Color
pattern Green = Color 0x27 0xae 0x60

pattern Yellow :: Color
pattern Yellow = Color 0xf3 0x9c 0x12

pattern Blue :: Color
pattern Blue = Color 0x29 0x80 0xb9

pattern Magenta :: Color
pattern Magenta = Color 0x8e 0x44 0xad

pattern Cyan :: Color
pattern Cyan = Color 0x16 0xa0 0x85

pattern White :: Color
pattern White = Color 0xec 0xf0 0xf1

pattern BrightBlack :: Color
pattern BrightBlack = Color 0x7f 0x8c 0x8d

pattern BrightRed :: Color
pattern BrightRed = Color 0xe7 0x4c 0x3c

pattern BrightGreen :: Color
pattern BrightGreen = Color 0x2e 0xcc 0x71

pattern BrightYellow :: Color
pattern BrightYellow = Color 0xf1 0xc4 0x0f

pattern BrightBlue :: Color
pattern BrightBlue = Color 0x34 0x98 0xdb

pattern BrightMagenta :: Color
pattern BrightMagenta = Color 0x9b 0x59 0xb6

pattern BrightCyan :: Color
pattern BrightCyan = Color 0x1a 0xbc 0x9c

pattern BrightWhite :: Color
pattern BrightWhite = Color 0xbd 0xc3 0xc7

pattern Default :: Color
pattern Default = Color 0x55 0x55 0x55

{- | ANSI escape code for a colour. The 16 named slots (plus 'Default') return
their canonical code; any other colour maps to the nearest ANSI slot.
-}
ansiCode :: Color -> Int
ansiCode Black = 30
ansiCode Red = 31
ansiCode Green = 32
ansiCode Yellow = 33
ansiCode Blue = 34
ansiCode Magenta = 35
ansiCode Cyan = 36
ansiCode White = 37
ansiCode BrightBlack = 90
ansiCode BrightRed = 91
ansiCode BrightGreen = 92
ansiCode BrightYellow = 93
ansiCode BrightBlue = 94
ansiCode BrightMagenta = 95
ansiCode BrightCyan = 96
ansiCode BrightWhite = 97
ansiCode Default = 39
ansiCode (Color r g b) = nearestAnsiCode r g b

{- | Map an arbitrary RGB colour to the nearest of the 16 ANSI slots, using the
canonical (VGA) ANSI palette as anchors — kept consistent with the terminal
chrome quantisation in "Granite.Render.Chrome".
-}
nearestAnsiCode :: Word8 -> Word8 -> Word8 -> Int
nearestAnsiCode r g b =
    snd
        (minimumBy (comparing fst) [(dist anchor, code) | (anchor, code) <- ansiAnchors])
  where
    ri = fromIntegral r :: Int
    gi = fromIntegral g
    bi = fromIntegral b
    dist (ar, ag, ab) = sq (ar - ri) + sq (ag - gi) + sq (ab - bi)
    sq x = x * x

ansiAnchors :: [((Int, Int, Int), Int)]
ansiAnchors =
    [ ((0, 0, 0), 30)
    , ((170, 0, 0), 31)
    , ((0, 170, 0), 32)
    , ((170, 85, 0), 33)
    , ((0, 0, 170), 34)
    , ((170, 0, 170), 35)
    , ((0, 170, 170), 36)
    , ((170, 170, 170), 37)
    , ((85, 85, 85), 90)
    , ((255, 85, 85), 91)
    , ((85, 255, 85), 92)
    , ((255, 255, 85), 93)
    , ((85, 85, 255), 94)
    , ((255, 85, 255), 95)
    , ((85, 255, 255), 96)
    , ((255, 255, 255), 97)
    ]

-- | Opening ANSI escape sequence selecting a colour (its nearest ANSI slot).
ansiOn :: Color -> Text
ansiOn c = "\ESC[" <> Text.pack (show (ansiCode c)) <> "m"

-- | ANSI reset escape sequence.
ansiOff :: Text
ansiOff = "\ESC[0m"

-- | Wrap a character in ANSI on/off sequences. Spaces pass through unchanged.
paint :: Color -> Char -> Text
paint c ch = if ch == ' ' then " " else ansiOn c <> Text.singleton ch <> ansiOff

-- | Exact @#rrggbb@ rendering of a colour, used by the SVG backend.
colorHex :: Color -> Text
colorHex (Color r g b) = "#" <> hex2 r <> hex2 g <> hex2 b
  where
    hex2 :: Word8 -> Text
    hex2 w =
        let s = showHex w ""
         in Text.pack (if length s < 2 then '0' : s else s)

{- | Parse a @"#rrggbb"@ (or @"rrggbb"@) string into a 'Color'. Returns
'Nothing' on malformed input.
-}
parseHex :: Text -> Maybe Color
parseHex t =
    case Text.unpack (Text.dropWhile (== '#') t) of
        cs@[r1, r2, g1, g2, b1, b2]
            | all isHexDigit cs ->
                Just (Color (byte r1 r2) (byte g1 g2) (byte b1 b2))
        _ -> Nothing
  where
    byte hi lo = fromIntegral (digitToInt hi * 16 + digitToInt lo)

-- | Default palette for layered charts (scatter, line, bars, …).
paletteColors :: [Color]
paletteColors =
    [ BrightBlue
    , BrightMagenta
    , BrightCyan
    , BrightGreen
    , BrightYellow
    , BrightRed
    , BrightWhite
    , BrightBlack
    ]

-- | Palette used by pie / box plots (categorical, more saturated).
pieColors :: [Color]
pieColors =
    [ BrightRed
    , BrightGreen
    , BrightYellow
    , BrightBlue
    , BrightMagenta
    , BrightCyan
    , BrightWhite
    , BrightBlack
    ]
