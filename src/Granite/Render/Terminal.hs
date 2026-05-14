{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

{- |
Module      : Granite.Render.Terminal
Copyright   : (c) 2025
License     : MIT
Maintainer  : mschavinda@gmail.com

Terminal backend.
-}
module Granite.Render.Terminal (
    renderScene,
    Canvas (..),
    Array2D,
    newCanvas,
    setDotC,
    fillDotsC,
    lineDotsC,
    renderCanvas,
    toBit,
    getA2D,
    setA2D,
    newA2D,
) where

import Data.Bits ((.|.))
import Data.Char (chr)
import Data.List qualified as List
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text

import Granite.Color (Color, ansiOff, ansiOn, paint)
import Granite.Internal.Util (setAt, updateAt)
import Granite.Render.Scene (
    Mark (..),
    Point (..),
    Rect (..),
    Scene (..),
    Style (..),
    TextAnchor (..),
    TextStyle (..),
    pxPerChar,
    pxPerLine,
 )

renderScene :: Scene -> Text
renderScene scene =
    let wChars = max 1 (ceiling (sceneWidth scene / pxPerChar))
        hChars = max 1 (ceiling (sceneHeight scene / pxPerLine))
        grid0 = replicate hChars (replicate wChars (' ', Nothing :: Maybe Color))
        canvas0 = newCanvas wChars hChars
        (grid1, canvas1) = List.foldl' (drawMark wChars hChars) (grid0, canvas0) (sceneMarks scene)
        grid2 = mergeCanvas grid1 canvas1
     in Text.unlines (map renderRuns grid2)

mergeCanvas :: [[(Char, Maybe Color)]] -> Canvas -> [[(Char, Maybe Color)]]
mergeCanvas grid canvas =
    [ zipWith merge row [0 :: Int ..]
    | (y, row) <- zip [0 :: Int ..] grid
    , let merge (ch, mc) x =
            case (ch, mc) of
                (' ', Nothing) ->
                    let bits = getA2D (buffer canvas) x y
                        col = getA2D (cbuf canvas) x y
                        glyph = if bits == 0 then ' ' else chr (0x2800 + bits)
                     in (glyph, col)
                _ -> (ch, mc)
    ]

drawMark ::
    Int ->
    Int ->
    ([[(Char, Maybe Color)]], Canvas) ->
    Mark ->
    ([[(Char, Maybe Color)]], Canvas)
drawMark wChars hChars (grid, canvas) mark = case mark of
    MRect (Rect x y w h) sty ->
        let col = styleFill sty
            cx0 = clampInt 0 (wChars - 1) (floor (x / pxPerChar))
            cy0 = clampInt 0 (hChars - 1) (floor (y / pxPerLine))
            cx1 = clampInt 0 (wChars - 1) (floor ((x + w - 1) / pxPerChar))
            cy1 = clampInt 0 (hChars - 1) (floor ((y + h - 1) / pxPerLine))
            grid' =
                List.foldl'
                    ( \g cy ->
                        List.foldl'
                            (\g' cx -> setCell g' cx cy ('█', col))
                            g
                            [cx0 .. cx1]
                    )
                    grid
                    [cy0 .. cy1]
         in (grid', canvas)
    MText (Point x y) txt ts ->
        let cy = clampInt 0 (hChars - 1) (floor (y / pxPerLine))
            width = Text.length txt
            startCol = case textAnchor ts of
                AnchorStart -> floor (x / pxPerChar)
                AnchorMiddle -> floor (x / pxPerChar) - width `div` 2
                AnchorEnd -> floor (x / pxPerChar) - width
            grid' = placeChars grid wChars cy startCol (Text.unpack txt) (textFill ts)
         in (grid', canvas)
    MCircle (Point x y) r sty ->
        let col = styleFill sty
            xDotC = round (x * 2 / pxPerChar)
            yDotC = round (y * 4 / pxPerLine)
            rDotX = max 1 (round (r * 2 / pxPerChar))
            rDotY = max 1 (round (r * 4 / pxPerLine))
            canvas' =
                fillDotsC
                    (xDotC - rDotX, yDotC - rDotY)
                    (xDotC + rDotX, yDotC + rDotY)
                    ( \dx dy ->
                        let ddx = fromIntegral (dx - xDotC) / fromIntegral rDotX :: Double
                            ddy = fromIntegral (dy - yDotC) / fromIntegral rDotY :: Double
                         in ddx * ddx + ddy * ddy <= 1
                    )
                    col
                    canvas
         in (grid, canvas')
    MPolyline pts sty ->
        let col = styleFill sty
            toDot (Point x y) =
                ( round (x * 2 / pxPerChar)
                , round (y * 4 / pxPerLine)
                )
            dots = map toDot pts
            pairs = zip dots (drop 1 dots)
            canvas' =
                List.foldl'
                    (\c ((x0, y0), (x1, y1)) -> lineDotsC (x0, y0) (x1, y1) col c)
                    canvas
                    pairs
         in (grid, canvas')
    MPolygon pts sty ->
        let stroke = case styleStroke sty of
                Just c -> Just c
                Nothing -> styleFill sty
            outline =
                if null pts
                    then pts
                    else pts <> [head pts]
         in drawMark
                wChars
                hChars
                (grid, canvas)
                (MPolyline outline sty{styleStroke = stroke})
    MAxisLine (Point x1 y1) (Point x2 y2) sty ->
        let col = styleStroke sty
            grid' = drawAxisLine wChars hChars x1 y1 x2 y2 col grid
         in (grid', canvas)
    MArc (Point cx cy) r a0 a1 sty ->
        let nSeg = 32 :: Int
            ang i = a0 + (a1 - a0) * fromIntegral i / fromIntegral nSeg
            pts =
                [ Point (cx + r * cos (ang i)) (cy + r * sin (ang i))
                | i <- [0 .. nSeg]
                ]
         in drawMark wChars hChars (grid, canvas) (MPolyline pts sty)
    MPath _ _ -> (grid, canvas)
    MGroup ms ->
        List.foldl' (drawMark wChars hChars) (grid, canvas) ms

placeChars ::
    [[(Char, Maybe Color)]] ->
    Int ->
    Int ->
    Int ->
    String ->
    Color ->
    [[(Char, Maybe Color)]]
placeChars grid wChars cy startCol s col
    | cy < 0 || cy >= length grid = grid
    | otherwise =
        let row0 = grid !! cy
            row' = applyChars row0 wChars startCol s
         in setAt grid cy row'
  where
    applyChars row _ _ [] = row
    applyChars row w c (ch : rest)
        | c < 0 || c >= w = applyChars row w (c + 1) rest
        | otherwise =
            let row' = setAt row c (ch, Just col)
             in applyChars row' w (c + 1) rest

setCell ::
    [[(Char, Maybe Color)]] ->
    Int ->
    Int ->
    (Char, Maybe Color) ->
    [[(Char, Maybe Color)]]
setCell grid cx cy cell =
    updateAt grid cy (\row -> setAt row cx cell)

clampInt :: Int -> Int -> Int -> Int
clampInt low high x = max low (min high x)

drawAxisLine ::
    Int ->
    Int ->
    Double ->
    Double ->
    Double ->
    Double ->
    Maybe Color ->
    [[(Char, Maybe Color)]] ->
    [[(Char, Maybe Color)]]
drawAxisLine wChars hChars x1 y1 x2 y2 col grid
    | abs (y2 - y1) < 1e-6 =
        let cy = clampInt 0 (hChars - 1) (floor (y1 / pxPerLine))
            cxL = clampInt 0 (wChars - 1) (floor (min x1 x2 / pxPerChar))
            cxR = clampInt 0 (wChars - 1) (floor (max x1 x2 / pxPerChar))
         in List.foldl' (\g cx -> writeAxisCell g cx cy '─' col) grid [cxL .. cxR]
    | abs (x2 - x1) < 1e-6 =
        let cx = clampInt 0 (wChars - 1) (floor (x1 / pxPerChar))
            cyT = clampInt 0 (hChars - 1) (floor (min y1 y2 / pxPerLine))
            cyB = clampInt 0 (hChars - 1) (floor (max y1 y2 / pxPerLine))
         in List.foldl' (\g cy -> writeAxisCell g cx cy '│' col) grid [cyT .. cyB]
    | otherwise = grid

writeAxisCell ::
    [[(Char, Maybe Color)]] ->
    Int ->
    Int ->
    Char ->
    Maybe Color ->
    [[(Char, Maybe Color)]]
writeAxisCell grid cx cy ch col =
    let existing = case grid `atRow` cy >>= (`atCol` cx) of
            Just (c, _) -> c
            Nothing -> ' '
        finalCh = combineAxis existing ch
     in setCell grid cx cy (finalCh, col)
  where
    atRow rs i
        | i < 0 || i >= length rs = Nothing
        | otherwise = Just (rs !! i)
    atCol cs i
        | i < 0 || i >= length cs = Nothing
        | otherwise = Just (cs !! i)

combineAxis :: Char -> Char -> Char
combineAxis existing new
    | existing == '│' && new == '─' = '┼'
    | existing == '─' && new == '│' = '┼'
    | existing == '┼' = '┼'
    | otherwise = new

renderRuns :: [(Char, Maybe Color)] -> Text
renderRuns = go
  where
    go [] = Text.empty
    go xs@((_, Nothing) : _) =
        let (plain, rest) = span (\(_, mc) -> case mc of Nothing -> True; _ -> False) xs
         in Text.pack (map fst plain) <> go rest
    go ((ch, Just c) : rest) =
        let (run, after) =
                span (\(_, mc) -> mc == Just c || isNothing mc) rest
            chunk = ch : map fst run
         in if all (== ' ') chunk
                then Text.pack chunk <> go after
                else ansiOn c <> Text.pack chunk <> ansiOff <> go after

data Array2D a = A2D Int Int (Arr a)

getA2D :: Array2D a -> Int -> Int -> a
getA2D (A2D w _ xs) x y = indexA xs (y * w + x)

setA2D :: Array2D a -> Int -> Int -> a -> Array2D a
setA2D (A2D w h xs) x y v =
    let i = y * w + x
     in A2D w h (setArr xs i v)

newA2D :: Int -> Int -> a -> Array2D a
newA2D w h v = A2D w h (fromList (replicate (w * h) v))

toBit :: Int -> Int -> Int
toBit ry rx = case (ry, rx) of
    (0, 0) -> 1
    (1, 0) -> 2
    (2, 0) -> 4
    (3, 0) -> 64
    (0, 1) -> 8
    (1, 1) -> 16
    (2, 1) -> 32
    (3, 1) -> 128
    _ -> 0

data Canvas = Canvas
    { cW :: Int
    , cH :: Int
    , buffer :: Array2D Int
    , cbuf :: Array2D (Maybe Color)
    }

newCanvas :: Int -> Int -> Canvas
newCanvas w h = Canvas w h (newA2D w h 0) (newA2D w h Nothing)

setDotC :: Canvas -> Int -> Int -> Maybe Color -> Canvas
setDotC c xDot yDot mcol
    | xDot < 0 || yDot < 0 || xDot >= cW c * 2 || yDot >= cH c * 4 = c
    | otherwise =
        let (cx, rx) = xDot `divMod` 2
            (cy, ry) = yDot `divMod` 4
            b = toBit ry rx
            m = getA2D (buffer c) cx cy
            c' = c{buffer = setA2D (buffer c) cx cy (m .|. b)}
         in case mcol of
                Nothing -> c'
                Just col -> c'{cbuf = setA2D (cbuf c) cx cy (Just col)}

fillDotsC ::
    (Int, Int) ->
    (Int, Int) ->
    (Int -> Int -> Bool) ->
    Maybe Color ->
    Canvas ->
    Canvas
fillDotsC (x0, y0) (x1, y1) p mcol c0 =
    let xs = [max 0 x0 .. min (cW c0 * 2 - 1) x1]
        ys = [max 0 y0 .. min (cH c0 * 4 - 1) y1]
     in List.foldl'
            (\c y -> List.foldl' (\c' x -> if p x y then setDotC c' x y mcol else c') c xs)
            c0
            ys

lineDotsC :: (Int, Int) -> (Int, Int) -> Maybe Color -> Canvas -> Canvas
lineDotsC (x0, y0) (x1, y1) mcol c0 =
    let dx = abs (x1 - x0)
        sx = if x0 < x1 then 1 else -1
        dy = negate (abs (y1 - y0))
        sy = if y0 < y1 then 1 else -1
        go x y err c
            | x == x1 && y == y1 = setDotC c x y mcol
            | otherwise =
                let e2 = 2 * err
                    (x', err') = if e2 >= dy then (x + sx, err + dy) else (x, err)
                    (y', err'') = if e2 <= dx then (y + sy, err' + dx) else (y, err')
                 in go x' y' err'' (setDotC c x y mcol)
     in go x0 y0 (dx + dy) c0

renderCanvas :: Canvas -> Text
renderCanvas (Canvas w h a colA) =
    let glyph 0 = ' '
        glyph m = chr (0x2800 + m)
        rows =
            fmap
                ( \y -> flip fmap [0 .. w - 1] $ \x ->
                    let m = getA2D a x y
                        ch = glyph m
                        mc = getA2D colA x y
                     in maybe (Text.singleton ch) (`paint` ch) mc
                )
                [0 .. h - 1]
     in Text.unlines (fmap Text.concat rows)

data Arr a
    = E
    | N Int Int (Arr a) a (Arr a)

sizeA :: Arr a -> Int
sizeA E = 0
sizeA (N sz _ _ _ _) = sz

heightA :: Arr a -> Int
heightA E = 0
heightA (N _ h _ _ _) = h

mk :: Arr a -> a -> Arr a -> Arr a
mk l x r = N sz h l x r
  where
    sl = sizeA l
    sr = sizeA r
    hl = heightA l
    hr = heightA r
    sz = 1 + sl + sr
    h = 1 + max hl hr

rotateL :: Arr a -> Arr a
rotateL (N _ _ l x (N _ _ rl y rr)) = mk (mk l x rl) y rr
rotateL _ = error "rotateL: malformed tree"

rotateR :: Arr a -> Arr a
rotateR (N _ _ (N _ _ ll y lr) x r) = mk ll y (mk lr x r)
rotateR _ = error "rotateR: malformed tree"

balance :: Arr a -> Arr a
balance t@(N _ _ l x r)
    | heightA l > heightA r + 1 =
        case l of
            N _ _ ll _ lr ->
                if heightA ll >= heightA lr
                    then rotateR t
                    else rotateR (mk (rotateL l) x r)
            _ -> t
    | heightA r > heightA l + 1 =
        case r of
            N _ _ rl _ rr ->
                if heightA rr >= heightA rl
                    then rotateL t
                    else rotateL (mk l x (rotateR r))
            _ -> t
    | otherwise = mk l x r
balance t = t

indexA :: Arr a -> Int -> a
indexA t i =
    case t of
        E -> error ("index out of bounds: " <> show i)
        N _ _ l x r ->
            let sl = sizeA l
             in if i < 0 || i >= 1 + sl + sizeA r
                    then error ("index out of bounds: " <> show i)
                    else
                        if i < sl
                            then indexA l i
                            else
                                if i == sl
                                    then x
                                    else indexA r (i - sl - 1)

setArr :: Arr a -> Int -> a -> Arr a
setArr t i y =
    case t of
        E -> error ("index out of bounds when setting: " <> show i)
        N _ _ l x r ->
            let sl = sizeA l
             in if i < 0 || i >= 1 + sl + sizeA r
                    then error ("index out of bounds: " <> show i)
                    else
                        if i < sl
                            then balance (mk (setArr l i y) x r)
                            else
                                if i == sl
                                    then mk l y r
                                    else balance (mk l x (setArr r (i - sl - 1) y))

fromList :: [a] -> Arr a
fromList xs = fst (build (length xs) xs)
  where
    build :: Int -> [a] -> (Arr a, [a])
    build 0 ys = (E, ys)
    build n ys =
        let (l, ys1) = build (n `div` 2) ys
            (x, ys2) = case ys1 of
                [] -> error "IMPOSSIBLE"
                (v : vs) -> (v, vs)
            (r, ys3) = build (n - n `div` 2 - 1) ys2
         in (mk l x r, ys3)
