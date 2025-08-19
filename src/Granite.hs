{-# LANGUAGE BangPatterns #-}

module Granite
  (
    Plot(..), defPlot, LegendPos(..)
  , series
  , bins, histogram
  , bars
  , scatter
  , pie
  ) where

import Data.Char (chr)
import Data.List (foldl', intercalate, intersperse, dropWhileEnd)
import Numeric (showFFloat, showEFloat)
import Data.Bits ((.&.), (.|.), xor)

data LegendPos = LegendRight | LegendBottom deriving (Eq, Show)

data Plot = Plot
  { widthChars   :: !Int
  , heightChars  :: !Int
  , leftMargin   :: !Int
  , bottomMargin :: !Int
  , titleMargin  :: !Int
  , legendPos    :: !LegendPos
  } deriving (Eq, Show)

defPlot :: Plot
defPlot = Plot
  { widthChars   = 60
  , heightChars  = 20
  , leftMargin   = 6
  , bottomMargin = 2
  , titleMargin  = 1
  , legendPos    = LegendRight
  }

data Color
  = Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue
  | BrightMagenta | BrightCyan | BrightWhite
  deriving (Eq, Show)

ansiCode :: Color -> Int
ansiCode Black         = 30
ansiCode Red           = 31
ansiCode Green         = 32
ansiCode Yellow        = 33
ansiCode Blue          = 34
ansiCode Magenta       = 35
ansiCode Cyan          = 36
ansiCode White         = 37
ansiCode BrightBlack   = 90
ansiCode BrightRed     = 91
ansiCode BrightGreen   = 92
ansiCode BrightYellow  = 93
ansiCode BrightBlue    = 94
ansiCode BrightMagenta = 95
ansiCode BrightCyan    = 96
ansiCode BrightWhite   = 97
ansiCode Default       = 39

ansiOn :: Color -> String
ansiOn c = "\ESC[" ++ show (ansiCode c) ++ "m"
ansiOff :: String
ansiOff = "\ESC[0m"

paint :: Color -> Char -> String
paint c ch = if ch == ' ' then " " else ansiOn c ++ [ch] ++ ansiOff

paletteColors :: [Color]
paletteColors =
  [ BrightBlue, BrightMagenta, BrightCyan, BrightGreen
  , BrightYellow, BrightRed, BrightWhite, BrightBlack
  ]

pieColors :: [Color]
pieColors =
  [ BrightRed, BrightGreen, BrightYellow, BrightBlue
  , BrightMagenta, BrightCyan, BrightWhite, BrightBlack
  ]

data Pat = Solid | Checker | DiagA | DiagB | Sparse deriving (Eq, Show)

ink :: Pat -> Int -> Int -> Bool
ink Solid  _  _  = True
ink Checker x  y = ((x `xor` y) .&. 1) == 0
ink DiagA  x  y = (x + y) `mod` 3 /= 1
ink DiagB  x  y = (x - y) `mod` 3 /= 1
ink Sparse x  y = (x .&. 1 == 0) && (y `mod` 3 == 0)

palette :: [Pat]
palette = [Solid, Checker, DiagA, DiagB, Sparse]

data Array2D a = A2D !Int !Int ![a]

getA2D :: Array2D a -> Int -> Int -> a
getA2D (A2D w _ xs) x y = xs !! (y*w + x)

setA2D :: Array2D a -> Int -> Int -> a -> Array2D a
setA2D (A2D w h xs) x y v =
  let i = y*w + x
  in A2D w h (take i xs ++ v : drop (i+1) xs)

newA2D :: Int -> Int -> a -> Array2D a
newA2D w h v = A2D w h (replicate (w*h) v)

toBit :: Int -> Int -> Int
toBit ry rx = case (ry,rx) of
  (0,0) -> 1
  (1,0) -> 2
  (2,0) -> 4
  (3,0) -> 64
  (0,1) -> 8
  (1,1) -> 16
  (2,1) -> 32
  (3,1) -> 128
  _     -> 0

data Canvas = Canvas
  { cw     :: !Int
  , ch     :: !Int
  , buffer :: !(Array2D Int)
  , cbuf   :: !(Array2D (Maybe Color))
  }

newCanvas :: Int -> Int -> Canvas
newCanvas w h = Canvas w h (newA2D w h 0) (newA2D w h Nothing)

setDotC :: Canvas -> Int -> Int -> Maybe Color -> Canvas
setDotC c xDot yDot mcol
  | xDot < 0 || yDot < 0 || xDot >= cw c * 2 || yDot >= ch c * 4 = c
  | otherwise =
      let cx = xDot `div` 2
          cy = yDot `div` 4
          rx = xDot - 2*cx
          ry = yDot - 4*cy
          b  = toBit ry rx
          m  = getA2D (buffer c) cx cy
          c' = c { buffer = setA2D (buffer c) cx cy (m .|. b) }
      in case mcol of
           Nothing -> c'
           Just col -> c' { cbuf = setA2D (cbuf c) cx cy (Just col) }

setDot :: Canvas -> Int -> Int -> Canvas
setDot c x y = setDotC c x y Nothing

fillDotsC :: (Int,Int) -> (Int,Int) -> (Int -> Int -> Bool) -> Maybe Color -> Canvas -> Canvas
fillDotsC (x0,y0) (x1,y1) p mcol c0 =
  let xs = [max 0 x0 .. min (cw c0*2-1) x1]
      ys = [max 0 y0 .. min (ch c0*4-1) y1]
  in  foldl' (\c y -> foldl' (\c' x -> if p x y then setDotC c' x y mcol else c') c xs) c0 ys

lineDotsC :: (Int,Int) -> (Int,Int) -> Maybe Color -> Canvas -> Canvas
lineDotsC (x0,y0) (x1,y1) mcol c0 =
  let dx = abs (x1 - x0)
      sx = if x0 < x1 then 1 else -1
      dy = negate (abs (y1 - y0))
      sy = if y0 < y1 then 1 else -1
      go !x !y !err c
        | x == x1 && y == y1 = setDotC c x y mcol
        | otherwise =
            let e2 = 2*err
                (x', err') = if e2 >= dy then (x + sx, err + dy) else (x, err)
                (y', err'')= if e2 <= dx then (y + sy, err' + dx) else (y, err')
            in go x' y' err'' (setDotC c x y mcol)
  in go x0 y0 (dx + dy) c0

renderCanvas :: Canvas -> String
renderCanvas (Canvas w h a colA) =
  let glyph 0 = ' '
      glyph m = chr (0x2800 + m)
      rows = [ [ let m = getA2D a x y
                     ch = glyph m
                     mc = getA2D colA x y
                 in maybe [ch] (\c -> paint c ch) mc
               | x <- [0..w-1] ]
             | y <- [0..h-1] ]
  in unlines (map concat rows)

justifyRight :: Int -> String -> String
justifyRight n s = replicate (max 0 (n - wcswidth s)) ' ' ++ s

wcswidth :: String -> Int
wcswidth = go 0
  where
    go !acc [] = acc
    go !acc ('\ESC':'[':rest) = let rest' = dropWhile (\c -> c /= 'm') rest
                                in case rest' of
                                     []     -> acc
                                     (_:xs) -> go acc xs
    go !acc (_:xs) = go (acc+1) xs

fmt :: Double -> String
fmt v
  | abs v >= 1000 || (abs v < 0.01 && v /= 0) = strip (showEFloat (Just 1) v "")
  | otherwise                                 = strip (showFFloat (Just 1) v "")
  where
    strip s = s

drawFrame :: Plot -> String -> String -> String -> String
drawFrame _cfg titleStr contentWithAxes legendBlockStr =
  unlines $ filter (not . null)
    ( [titleStr | not (null titleStr)]
   ++ [contentWithAxes]
   ++ [legendBlockStr | not (null legendBlockStr)] )

axisify :: Plot -> Canvas -> (Double,Double) -> (Double,Double) -> String
axisify cfg c (xmin,xmax) (ymin,ymax) =
  let plotW = cw c
      plotH = ch c
      left  = leftMargin cfg
      pad   = replicate left ' '

      yTicks  = [(0, ymax), (plotH `div` 2, (ymin+ymax)/2), (plotH-1, ymin)]
      baseLbl = replicate plotH pad
      setAt xs i v | i < 0 || i >= length xs = xs
                   | otherwise               = take i xs ++ v : drop (i+1) xs
      yLabels = foldl' (\acc (row,v) -> setAt acc row (justifyRight left (fmt v)))
                       baseLbl yTicks

      canvasLines = lines (renderCanvas c)
      attachY = zipWith (\lbl line -> lbl ++ "│" ++ line) yLabels canvasLines

      xBar   = pad ++ "│" ++ replicate plotW '─'
      xLbls  = [(0, xmin), (plotW `div` 2, (xmin+xmax)/2), (plotW-1, xmax)]
      xLine  = placeLabels (replicate (left + 1 + plotW) ' ') (left + 1)
                           [ (x, fmt v) | (x,v) <- xLbls ]
  in unlines (attachY ++ [xBar, xLine])

axisifyGrid :: Plot -> [[(Char, Maybe Color)]] -> (Double,Double) -> (Double,Double) -> String
axisifyGrid cfg grid (xmin,xmax) (ymin,ymax) =
  let plotH = length grid
      plotW = if null grid then 0 else length (head grid)
      left  = leftMargin cfg
      pad   = replicate left ' '

      yTicks  = [(0, ymax), (plotH `div` 2, (ymin+ymax)/2), (plotH-1, ymin)]
      baseLbl = replicate plotH pad
      setAt xs i v | i < 0 || i >= length xs = xs
                   | otherwise               = take i xs ++ v : drop (i+1) xs
      yLabels = foldl' (\acc (row,v) -> setAt acc row (justifyRight left (fmt v)))
                       baseLbl yTicks

      renderRow cells = concat [ maybe [ch] (\c -> paint c ch) mc | (ch, mc) <- cells ]
      attachY = zipWith (\lbl cells -> lbl ++ "│" ++ renderRow cells) yLabels grid

      xBar   = pad ++ "│" ++ replicate plotW '─'
      xLbls  = [(0, xmin), (plotW `div` 2, (xmin+xmax)/2), (plotW-1, xmax)]
      xLine  = placeLabels (replicate (left + 1 + plotW) ' ') (left + 1)
                           [ (x, fmt v) | (x,v) <- xLbls ]
  in unlines (attachY ++ [xBar, xLine])

placeLabels :: String -> Int -> [(Int,String)] -> String
placeLabels base off xs = foldl' place base xs
  where
    place acc (x,s) =
      let i = off + x
      in take i acc ++ s ++ drop (i + wcswidth s) acc

legendBlock :: LegendPos -> Int -> [(String, Pat, Color)] -> String
legendBlock LegendBottom width entries =
  let cells = [ sample pat col ++ " " ++ name | (name, pat, col) <- entries ]
      line  = intercalate "   " cells
      pad   = let vis = wcswidth line
              in if vis < width then replicate ((width - vis) `div` 2) ' ' else ""
  in pad ++ line
legendBlock LegendRight _ entries =
  unlines [ sample pat col ++ " " ++ name | (name, pat, col) <- entries ]

sample :: Pat -> Color -> String
sample p col =
  let c = foldl' (\cv (dx,dy) -> if ink p dx dy then setDotC cv (dx `mod` 2) (dy `mod` 4) (Just col) else cv)
                 (newCanvas 1 1)
                 [(x,y) | y <- [0..3], x <- [0..1]]
      s = renderCanvas c
  in dropWhileEnd (== '\n') s

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

eps :: Double
eps = 1e-12

boundsXY :: [(Double,Double)] -> (Double,Double,Double,Double)
boundsXY pts =
  let xs = map fst pts; ys = map snd pts
      xmin = minimum xs; xmax = maximum xs
      ymin = minimum ys; ymax = maximum ys
      padx = (xmax - xmin) * 0.05 + 1e-9
      pady = (ymax - ymin) * 0.05 + 1e-9
  in (xmin - padx, xmax + padx, ymin - pady, ymax + pady)

mod' :: Double -> Double -> Double
mod' a m = a - fromIntegral (floor (a / m)) * m

series :: String -> [(Double,Double)] -> (String, [(Double,Double)])
series = (,)

scatter :: String -> [(String, [(Double,Double)])] -> Plot -> IO String
scatter title sers cfg = pure $
  let wC = widthChars cfg; hC = heightChars cfg
      plotC = newCanvas wC hC
      (xmin,xmax,ymin,ymax) = boundsXY (concatMap snd sers)
      sx x = clamp 0 (wC*2-1)  $ round ((x - xmin) / (xmax - xmin + eps) * fromIntegral (wC*2-1))
      sy y = clamp 0 (hC*4-1)  $ round ((ymax - y) / (ymax - ymin + eps) * fromIntegral (hC*4-1))
      pats = cycle palette
      cols = cycle paletteColors
      withSty = zipWith3 (\(n,ps) p c -> (n,ps,p,c)) sers pats cols
      drawOne (_name, pts, pat, col) c0 =
        foldl' (\c (x,y) -> let xd = sx x; yd = sy y
                            in if ink pat xd yd then setDotC c xd yd (Just col) else c)
               c0 pts
      cDone = foldl' (flip drawOne) plotC withSty
      ax    = axisify cfg cDone (xmin,xmax) (ymin,ymax)
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [ (n,p, col) | (n,_,p,col) <- withSty ]
      titled = if null title then "" else title
  in drawFrame cfg titled ax legend


blockChar :: Int -> Char
blockChar n = case clamp 0 8 n of
  0->' '; 1->'▁'; 2->'▂'; 3->'▃'; 4->'▄'; 5->'▅'; 6->'▆'; 7->'▇'; _->'█'

colGlyphs :: Int -> Double -> String
colGlyphs hC frac =
  let total = hC * 8
      ticks = clamp 0 total (round (frac * fromIntegral total))
      full  = ticks `div` 8
      rem8  = ticks - full*8
      topPad = hC - full - (if rem8>0 then 1 else 0)
      middle = [blockChar rem8 | rem8 > 0]
  in replicate topPad ' ' ++ middle ++ replicate full '█'

resampleToWidth :: Int -> [Double] -> [Double]
resampleToWidth w xs
  | w <= 0    = []
  | null xs   = replicate w 0
  | n == w    = xs
  | n >  w    = avgGroup (ceiling (fromIntegral n / fromIntegral w))
  | otherwise = replicateOut
  where
    n = length xs
    avgGroup g =
      [ avg (take g (drop (i*g) xs)) | i <- [0..w-1] ]
      where avg ys = if null ys then 0 else sum ys / fromIntegral (length ys)
    replicateOut =
      let base  = w `div` n
          extra = w - base * n
      in concat [ replicate (base + (if i < extra then 1 else 0)) v
                | (i,v) <- zip [0..] xs ]

data Bins = Bins { nBins :: !Int, lo :: !Double, hi :: !Double } deriving (Eq, Show)
bins :: Int -> Double -> Double -> Bins
bins n a b = Bins (max 1 n) (min a b) (max a b)

histogram :: String -> Bins -> [Double] -> Plot -> IO String
histogram title (Bins n a b) xs cfg = pure $
  let step    = (b - a) / fromIntegral n
      binIx x = clamp 0 (n-1) $ floor ((x - a) / step)
      counts  = foldl' (\acc x ->
                          if x < a || x > b then acc
                          else addAt acc (binIx x) 1)
                       (replicate n 0 :: [Int]) xs
      maxC    = fromIntegral (maximum (1:counts))
      fracs0  = [ fromIntegral c / maxC | c <- counts ]

      wData   = widthChars cfg
      hC      = heightChars cfg
      colsF   = resampleToWidth wData fracs0

      dataCols  = [ (colGlyphs hC f, Just BrightCyan) | f <- colsF ]
      gutterCol = (replicate hC ' ', Nothing)
      columns   = concat (intersperse [gutterCol] (map pure dataCols))

      grid :: [[(Char, Maybe Color)]]
      grid = [ [ (fst col !! y, snd col) | col <- columns ]
             | y <- [0 .. hC-1] ]

      ax     = axisifyGrid cfg grid (a,b) (0, fromIntegral (maximum (1:counts)))
      legendWidth = leftMargin cfg + 1 + (if null grid then 0 else length (head grid))
      legend = legendBlock (legendPos cfg) legendWidth [("count", Solid, BrightCyan)]
      titled = if null title then "" else title
  in drawFrame cfg titled ax legend

addAt :: [Int] -> Int -> Int -> [Int]
addAt xs i v = take i xs ++ [xs !! i + v] ++ drop (i+1) xs

bars :: String -> [(String, Double)] -> Plot -> IO String
bars title kvs cfg = pure $
  let wC   = widthChars cfg
      hC   = heightChars cfg
      vals = map snd kvs
      vmax = maximum (1e-12 : map abs vals)

      cats :: [(String, Double, Color)]
      cats = [ (name, abs v / vmax, col)
             | ((name, v), col) <- zip kvs (cycle paletteColors) ]

      nCats = length cats

      (base, extra) =
        if nCats == 0 then (0, 0) else (wC `div` nCats, wC - (wC `div` nCats) * nCats)
      widths = [ base + (if i < extra then 1 else 0) | i <- [0..nCats-1] ]

      catGroups :: [[(String, Maybe Color)]]
      catGroups =
        [ replicate w (colGlyphs hC f, Just col)
        | ((_, f, col), w) <- zip cats widths
        ]

      gutterCol = (replicate hC ' ', Nothing)
      columns   = concat (intersperse [gutterCol] catGroups)

      grid :: [[(Char, Maybe Color)]]
      grid = [ [ (glyphs !! y, mc) | (glyphs, mc) <- columns ]
             | y <- [0 .. hC-1] ]

      ax     = axisifyGrid cfg grid (0, fromIntegral (max 1 nCats)) (0, vmax)
      legendWidth = leftMargin cfg + 1 + (if null grid then 0 else length (head grid))
      legend = legendBlock (legendPos cfg) legendWidth
                 [ (name, Checker, col) | (name, _, col) <- cats ]
      titled = if null title then "" else title
  in drawFrame cfg titled ax legend

pie :: String -> [(String, Double)] -> Plot -> IO String
pie title parts0 cfg = pure $
  let parts = normalize parts0
      wC = widthChars cfg; hC = heightChars cfg
      plotC = newCanvas wC hC
      wDots = wC*2; hDots = hC*4
      r     = min (wDots `div` 2 - 2) (hDots `div` 2 - 2)
      cx    = wDots `div` 2
      cy    = hDots `div` 2
      toAng p = p * 2*pi
      wedges = scanl (\a (_,p) -> a + toAng p) 0 parts
      angles = zip wedges (tail wedges)
      names  = map fst parts
      cols   = cycle pieColors
      withP  = zipWith3 (\n ang col -> (n,ang,col)) names angles cols

      drawOne (_name,(a0,a1),col) c0 =
        let inside x y =
              let dx  = fromIntegral (x - cx)
                  dy  = fromIntegral (cy - y)
                  rr2 = dx*dx + dy*dy
                  r2  = fromIntegral (r*r)
                  ang = atan2 dy dx `mod'` (2*pi)
              in rr2 <= r2 && angleWithin ang a0 a1
        in fillDotsC (cx - r, cy - r) (cx + r, cy + r) (\x y -> inside x y) (Just col) c0

      cDone  = foldl' (flip drawOne) plotC withP
      ax     = axisify cfg cDone (0,1) (0,1)
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [ (n, Solid, col) | (n,_,col) <- withP ]
      titled= if null title then "" else title
  in drawFrame cfg titled ax legend

normalize :: [(String, Double)] -> [(String, Double)]
normalize xs =
  let s = sum (map (abs . snd) xs) + 1e-12
  in [ (n, max 0 (v / s)) | (n,v) <- xs ]

angleWithin :: Double -> Double -> Double -> Bool
angleWithin ang a0 a1
  | a1 >= a0  = ang >= a0 && ang <= a1
  | otherwise = ang >= a0 || ang <= a1
