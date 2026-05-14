{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Color
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Color primitives shared by both the terminal and SVG backends:
the ANSI 16-color palette ('Color'), escape-code helpers, and the
hex mapping used by the SVG backend.
-}
module Granite.Color (
    Color (..),
    ansiCode,
    ansiOn,
    ansiOff,
    paint,
    colorHex,
    paletteColors,
    pieColors,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Supported ANSI colo(u)rs.
data Color
    = Default
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite
    deriving (Eq, Show, Read)

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

ansiOn :: Color -> Text
ansiOn c = "\ESC[" <> Text.pack (show (ansiCode c)) <> "m"

ansiOff :: Text
ansiOff = "\ESC[0m"

-- | Wrap a character in ANSI on/off sequences. Spaces pass through unchanged.
paint :: Color -> Char -> Text
paint c ch = if ch == ' ' then " " else ansiOn c <> Text.singleton ch <> ansiOff

-- | Hex RGB approximation of the ANSI palette, used by the SVG backend.
colorHex :: Color -> Text
colorHex Default = "#555555"
colorHex Black = "#2c3e50"
colorHex Red = "#c0392b"
colorHex Green = "#27ae60"
colorHex Yellow = "#f39c12"
colorHex Blue = "#2980b9"
colorHex Magenta = "#8e44ad"
colorHex Cyan = "#16a085"
colorHex White = "#ecf0f1"
colorHex BrightBlack = "#7f8c8d"
colorHex BrightRed = "#e74c3c"
colorHex BrightGreen = "#2ecc71"
colorHex BrightYellow = "#f1c40f"
colorHex BrightBlue = "#3498db"
colorHex BrightMagenta = "#9b59b6"
colorHex BrightCyan = "#1abc9c"
colorHex BrightWhite = "#bdc3c7"

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
