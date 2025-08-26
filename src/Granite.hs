{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Granite
  (
    Plot(..), defPlot, LegendPos(..)
  , series
  , bins, histogram
  , bars
  , scatter
  , pie
  , stackedBars
  , heatmap
  , lineGraph
  , boxPlot
  ) where

import Data.Bits ((.&.), (.|.), xor)
import Data.Char (chr)
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showFFloat, showEFloat)
import Text.Printf

data LegendPos = LegendRight | LegendBottom deriving (Eq, Show)

data Plot = Plot
  { widthChars   :: Int
  , heightChars  :: Int
  , leftMargin   :: Int
  , bottomMargin :: Int
  , titleMargin  :: Int
  -- first element is xmin second is xmax.
  -- Similar scheme for yBounds.
  -- Note: Here nothing does not mean the axis is
  -- unbounded it means we fall back to the default bounding
  -- logic.
  , xBounds      :: (Maybe Double, Maybe Double)
  , yBounds      :: (Maybe Double, Maybe Double)
  , plotTitle    :: Text
  , legendPos    :: LegendPos
  } deriving (Eq, Show)

defPlot :: Plot
defPlot = Plot
  { widthChars   = 60
  , heightChars  = 20
  , leftMargin   = 6
  , bottomMargin = 2
  , titleMargin  = 1
  , xBounds      = (Nothing, Nothing)
  , yBounds      = (Nothing, Nothing)
  , plotTitle    = ""
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

ansiOn :: Color -> Text
ansiOn c = "\ESC[" <> Text.show (ansiCode c) <> "m"

ansiOff :: Text
ansiOff = "\ESC[0m"

paint :: Color -> Char -> Text
paint c ch = if ch == ' ' then " " else ansiOn c <> (Text.singleton ch) <> ansiOff

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

data Array2D a = A2D Int Int (Arr a)

getA2D :: Array2D a -> Int -> Int -> a
getA2D (A2D w _ xs) x y = indexA xs (y*w + x)

setA2D :: Array2D a -> Int -> Int -> a -> Array2D a
setA2D (A2D w h xs) x y v =
  let i = y*w + x
  in A2D w h (setA xs i v)

newA2D :: Int -> Int -> a -> Array2D a
newA2D w h v = A2D w h (fromList (replicate (w*h) v))

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
  { cW     :: Int
  , cH     :: Int
  , buffer :: (Array2D Int)
  , cbuf   :: (Array2D (Maybe Color))
  }

newCanvas :: Int -> Int -> Canvas
newCanvas w h = Canvas w h (newA2D w h 0) (newA2D w h Nothing)

setDotC :: Canvas -> Int -> Int -> Maybe Color -> Canvas
setDotC c xDot yDot mcol
  | xDot < 0 || yDot < 0 || xDot >= cW c * 2 || yDot >= cH c * 4 = c
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

fillDotsC :: (Int,Int) -> (Int,Int) -> (Int -> Int -> Bool) -> Maybe Color -> Canvas -> Canvas
fillDotsC (x0,y0) (x1,y1) p mcol c0 =
  let xs = [max 0 x0 .. min (cW c0*2-1) x1]
      ys = [max 0 y0 .. min (cH c0*4-1) y1]
  in  List.foldl' (\c y -> List.foldl' (\c' x -> if p x y then setDotC c' x y mcol else c') c xs) c0 ys

renderCanvas :: Canvas -> Text
renderCanvas (Canvas w h a colA) =
  let glyph 0 = ' ' 
      glyph m = chr (0x2800 + m)
      rows = flip fmap [0..h-1] (\y -> flip fmap [0..w-1] $ \x -> 
        let m = getA2D a x y
            ch = glyph m
            mc = getA2D colA x y
        in maybe (Text.singleton ch) (\c -> paint c ch) mc)
  in Text.unlines (fmap Text.concat rows)

justifyRight :: Int -> Text -> Text
justifyRight n s = Text.replicate (max 0 (n - wcswidth s)) " " <> s

wcswidth :: Text -> Int
wcswidth t = go 0 (Text.unpack t)
  where
    go acc [] = acc
    go acc ('\ESC':'[':rest) = let rest' = dropWhile (\c -> c /= 'm') rest
                                in case rest' of
                                     []     -> acc
                                     (_:xs) -> go acc xs
    go acc (_:xs) = go (acc+1) xs

fmt :: Double -> Text
fmt v
  | abs v >= 10000 || (abs v < 0.01 && v /= 0) = Text.pack (showEFloat (Just 1) v "")
  | otherwise                                  = Text.pack (showFFloat (Just 1) v "")

drawFrame :: Plot -> Text -> Text -> Text
drawFrame _cfg contentWithAxes legendBlockStr =
  Text.unlines $ filter (not . Text.null)
    ( [plotTitle _cfg | not (Text.null (plotTitle _cfg))]
   <> [contentWithAxes]
   <> [legendBlockStr | not (Text.null legendBlockStr)] )

axisify :: Plot -> Canvas -> (Double,Double) -> (Double,Double) -> Text
axisify cfg c (xmin,xmax) (ymin,ymax) =
  let plotW = cW c
      plotH = cH c
      left  = leftMargin cfg
      pad   = Text.replicate left " "

      yTicks  = [(0, ymax), (plotH `div` 2, (ymin+ymax)/2), (plotH-1, ymin)]
      baseLbl = replicate plotH pad

      setAt :: [Text] -> Int -> Text -> [Text]
      setAt xs i v | i < 0 || i >= length xs = xs
                   | otherwise = take i xs <> [v] <> drop (i+1) xs

      yLabels = List.foldl' (\acc (row,v) -> setAt acc row (justifyRight left (fmt v)))
                       baseLbl yTicks

      canvasLines = Text.lines (renderCanvas c)
      attachY :: [Text]
      attachY = zipWith (\lbl line -> lbl <> "│" <> line) yLabels canvasLines

      xBar   = pad <> "│" <> Text.replicate plotW "─"
      xLbls  = [(0, xmin), (plotW `div` 2, (xmin+xmax)/2), (plotW-1, xmax)]
      xLine  = placeLabels (Text.replicate (left + 1 + plotW) " ") (left + 1)
                           [ (x, fmt v) | (x,v) <- xLbls ]
  in Text.unlines (attachY <> [xBar, xLine])

axisifyGrid :: Plot -> [[(Char, Maybe Color)]] -> (Double,Double) -> (Double,Double) -> Text
axisifyGrid cfg grid (xmin,xmax) (ymin,ymax) =
  let plotH = length grid
      plotW = gridWidth grid
      left  = leftMargin cfg
      pad   = Text.replicate left " "

      yTicks :: [(Int, Double)]
      yTicks  = [(0, ymax), (plotH `div` 2, (ymin+ymax)/2), (plotH-1, ymin)]

      baseLbl :: [Text]
      baseLbl = List.replicate plotH pad

      setAt :: [Text] -> Int -> Text -> [Text]
      setAt xs i v | i < 0 || i >= length xs = xs
                   | otherwise = take i xs <> [v] <> drop (i+1) xs

      yLabels :: [Text]
      yLabels = List.foldl' 
          (\acc (row,v) -> setAt acc row (justifyRight left (fmt v)))
          baseLbl
          yTicks

      renderRow :: [(Char, Maybe Color)] -> Text
      renderRow cells = Text.concat $ 
        fmap (\(ch, mc) -> maybe (Text.singleton ch) (\c -> paint c ch) mc) cells

      attachY :: [Text]
      attachY = zipWith (\lbl cells -> lbl <> "│" <> renderRow cells) yLabels grid

      xBar :: Text
      xBar   = pad <> "│" <> Text.replicate plotW "─"
      xLbls  = [(0, xmin), (plotW `div` 2, (xmin+xmax)/2), (plotW-1, xmax)]
      xLine  = placeLabels 
                 (Text.replicate (left + 1 + plotW) " ")
                 (left + 1)
                 (fmap (\(x,v) -> (x, fmt v)) xLbls)
  in Text.unlines (attachY <> [xBar, xLine])

placeLabels :: Text -> Int -> [(Int,Text)] -> Text
placeLabels base off xs = List.foldl' place base xs
  where
    place :: Text -> (Int, Text) -> Text
    place acc (x,s) =
      let i = off + x
      in Text.take i acc <> s <> Text.drop (i + wcswidth s) acc

legendBlock :: LegendPos -> Int -> [(Text, Pat, Color)] -> Text
legendBlock LegendBottom width entries =
  let cells = [ sample pat col <> " " <> name | (name, pat, col) <- entries ]
      line  = Text.intercalate "   " cells
      pad   = let vis = wcswidth line
              in if vis < width then Text.replicate ((width - vis) `div` 2) " " else ""
  in pad <> line
legendBlock LegendRight _ entries =
  Text.unlines $
    fmap (\(name, pat, col) -> sample pat col <> " " <> name) entries

sample :: Pat -> Color -> Text
sample p col =
  let c = List.foldl' (\cv (dx,dy) -> if ink p dx dy then setDotC cv (dx `mod` 2) (dy `mod` 4) (Just col) else cv)
                 (newCanvas 1 1)
                 [(x,y) | y <- [0..3], x <- [0..1]]
      s = renderCanvas c
  in Text.dropWhileEnd (== '\n') s

clamp :: Ord a => a -> a -> a -> a
clamp low high x = max low (min high x)

eps :: Double
eps = 1e-12

boundsXY :: Plot -> [(Double,Double)] -> (Double,Double,Double,Double)
boundsXY cfg pts =
  let xs = map fst pts; ys = map snd pts
      xmin = minimum' xs; xmax = maximum' xs
      ymin = minimum' ys; ymax = maximum' ys
      padx = (xmax - xmin) * 0.05 + 1e-9
      pady = (ymax - ymin) * 0.05 + 1e-9
  in (fromMaybe (xmin - padx) (fst (xBounds cfg)),
      fromMaybe (xmax + padx) (snd (xBounds cfg)),
      fromMaybe (ymin - pady) (fst (yBounds cfg)),
      fromMaybe (ymax + pady) (snd (yBounds cfg)))

mod' :: Double -> Double -> Double
mod' a m = a - fromIntegral (floor (a / m) :: Int) * m

series :: Text -> [(Double,Double)] -> (Text, [(Double,Double)])
series = (,)

scatter :: [(Text, [(Double,Double)])] -> Plot -> Text
scatter sers cfg =
  let wC = widthChars cfg; hC = heightChars cfg
      plotC = newCanvas wC hC
      (xmin,xmax,ymin,ymax) = boundsXY cfg (concatMap snd sers)
      sx x = clamp 0 (wC*2-1)  $ round ((x - xmin) / (xmax - xmin + eps) * fromIntegral (wC*2-1))
      sy y = clamp 0 (hC*4-1)  $ round ((ymax - y) / (ymax - ymin + eps) * fromIntegral (hC*4-1))
      pats = cycle palette
      cols = cycle paletteColors
      withSty = zipWith3 (\(n,ps) p c -> (n,ps,p,c)) sers pats cols
      drawOne (_name, pts, pat, col) c0 =
        List.foldl' (\c (x,y) -> let xd = sx x; yd = sy y
                            in if ink pat xd yd then setDotC c xd yd (Just col) else c)
               c0 pts
      cDone = List.foldl' (flip drawOne) plotC withSty
      ax    = axisify cfg cDone (xmin,xmax) (ymin,ymax)
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [ (n,p, col) | (n,_,p,col) <- withSty ]
  in drawFrame cfg ax legend


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
  in replicate topPad ' ' <> middle <> replicate full '█'

resampleToWidth :: Int -> [Double] -> [Double]
resampleToWidth w xs
  | w <= 0    = []
  | null xs   = replicate w 0
  | n == w    = xs
  | n >  w    = avgGroup (ceiling (fromIntegral n / (fromIntegral w :: Double)))
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

data Bins = Bins 
  { nBins :: Int
  , lo :: Double
  , hi :: Double
  } deriving (Eq, Show)

bins :: Int -> Double -> Double -> Bins
bins n a b = Bins (max 1 n) (min a b) (max a b)

histogram :: Bins -> [Double] -> Plot -> Text
histogram (Bins n a b) xs cfg =
  let step    = (b - a) / fromIntegral n
      binIx x = clamp 0 (n-1) $ floor ((x - a) / step)
      counts  = List.foldl' (\acc x ->
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
      columns   = concat (List.intersperse [gutterCol] (map pure dataCols))

      grid :: [[(Char, Maybe Color)]]
      grid = [ [ (fst col !! y, snd col) | col <- columns ]
             | y <- [0 .. hC-1] ]

      ax     = axisifyGrid cfg grid (a,b) (0, fromIntegral (maximum (1:counts)))
      legendWidth = leftMargin cfg + 1 + (gridWidth grid)
      legend = legendBlock (legendPos cfg) legendWidth [("count", Solid, BrightCyan)]
  in drawFrame cfg ax legend

addAt :: [Int] -> Int -> Int -> [Int]
addAt xs i v = take i xs <> [xs !! i + v] <> drop (i+1) xs

bars :: [(Text, Double)] -> Plot -> Text
bars kvs cfg =
  let wC   = widthChars cfg
      hC   = heightChars cfg
      vals = map snd kvs
      vmax = maximum' (map abs vals)

      cats :: [(Text, Double, Color)]
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
      columns   = concat (List.intersperse [gutterCol] catGroups)

      grid :: [[(Char, Maybe Color)]]
      grid = [ [ (glyphs !! y, mc) | (glyphs, mc) <- columns ]
             | y <- [0 .. hC-1] ]

      ax     = axisifyGrid cfg grid (0, fromIntegral (max 1 nCats)) (0, vmax)
      legendWidth = leftMargin cfg + 1 + (gridWidth grid)
      legend = legendBlock (legendPos cfg) legendWidth
                 [ (name, Checker, col) | (name, _, col) <- cats ]
  in drawFrame cfg ax legend

pie :: [(Text, Double)] -> Plot -> Text
pie parts0 cfg =
  let parts = normalize parts0
      wC = widthChars cfg; hC = heightChars cfg
      plotC = newCanvas wC hC
      wDots = wC*2; hDots = hC*4
      r     = min (wDots `div` 2 - 2) (hDots `div` 2 - 2)
      cx    = wDots `div` 2
      cy    = hDots `div` 2
      toAng p = p * 2*pi
      wedges = scanl (\a (_,p) -> a + toAng p) 0 parts
      angles = zip wedges (drop 1 wedges)
      names  = map fst parts
      cols   = cycle pieColors
      withP :: [(Text, (Double, Double), Color)]
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

      cDone  = List.foldl' (flip drawOne) plotC withP
      ax     = axisify cfg cDone (0,1) (0,1)
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [ (n, Solid, col) | (n,_,col) <- withP ]
  in drawFrame cfg ax legend

normalize :: [(Text, Double)] -> [(Text, Double)]
normalize xs =
  let s = sum (map (abs . snd) xs) + 1e-12
  in [ (n, max 0 (v / s)) | (n,v) <- xs ]

angleWithin :: Double -> Double -> Double -> Bool
angleWithin ang a0 a1
  | a1 >= a0  = ang >= a0 && ang <= a1
  | otherwise = ang >= a0 || ang <= a1

lineDotsC :: (Int,Int) -> (Int,Int) -> Maybe Color -> Canvas -> Canvas
lineDotsC (x0,y0) (x1,y1) mcol c0 =
  let dx = abs (x1 - x0)
      sx = if x0 < x1 then 1 else -1
      dy = negate (abs (y1 - y0))
      sy = if y0 < y1 then 1 else -1
      go x y err c
        | x == x1 && y == y1 = setDotC c x y mcol
        | otherwise =
            let e2 = 2*err
                (x', err') = if e2 >= dy then (x + sx, err + dy) else (x, err)
                (y', err'')= if e2 <= dx then (y + sy, err' + dx) else (y, err')
            in go x' y' err'' (setDotC c x y mcol)
  in go x0 y0 (dx + dy) c0

lineGraph :: [(Text, [(Double,Double)])] -> Plot -> Text
lineGraph sers cfg =
  let wC = widthChars cfg; hC = heightChars cfg
      plotC = newCanvas wC hC
      (xmin,xmax,ymin,ymax) = boundsXY cfg (concatMap snd sers)
      sx x = clamp 0 (wC*2-1) $ round ((x - xmin) / (xmax - xmin + eps) * fromIntegral (wC*2-1))
      sy y = clamp 0 (hC*4-1) $ round ((ymax - y) / (ymax - ymin + eps) * fromIntegral (hC*4-1))
      
      cols = cycle paletteColors
      withSty = zip sers cols
      
      drawSeries ((_name, pts), col) c0 =
        let sortedPts = List.sortOn fst pts
            dotPairs = zip sortedPts (drop 1 sortedPts)
        in List.foldl' (\c ((x1,y1), (x2,y2)) -> 
                    lineDotsC (sx x1, sy y1) (sx x2, sy y2) (Just col) c)
                  c0 dotPairs
      
      cDone = List.foldl' (flip drawSeries) plotC withSty
      ax :: Text
      ax = axisify cfg cDone (xmin,xmax) (ymin,ymax)
      legend :: Text
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [(n, Solid, col) | ((n,_), col) <- withSty]
  in drawFrame cfg ax legend

quartiles :: [Double] -> (Double, Double, Double, Double, Double)
quartiles [] = (0, 0, 0, 0, 0) -- Idk. Maybe throw an error here???
quartiles xs = 
  let sorted = List.sort xs
      n = length sorted
      q1Idx = n `div` 4
      q2Idx = n `div` 2
      q3Idx = (3 * n) `div` 4
      getIdx i = if i < n then sorted !! i else last sorted
  in if n < 5 
     then let m = sum xs / fromIntegral n in (m,m,m,m,m)
     else (fromMaybe 0 (fmap fst (List.uncons sorted)), getIdx q1Idx, getIdx q2Idx, getIdx q3Idx, last sorted)

boxPlot :: [(Text, [Double])] -> Plot -> Text
boxPlot datasets cfg =
  let wC = widthChars cfg
      hC = heightChars cfg

      stats = [(name, quartiles vals) | (name, vals) <- datasets]

      allVals = concatMap snd datasets
      ymin = if null allVals then 0 else minimum' allVals - abs (minimum' allVals) * 0.1
      ymax = if null allVals then 1 else maximum' allVals + abs (maximum' allVals) * 0.1

      nBoxes = length datasets
      boxWidth = if nBoxes == 0 then 1 else max 1 (wC `div` (nBoxes * 2))
      spacing = if nBoxes <= 1 then 0 else (wC - boxWidth * nBoxes) `div` (nBoxes - 1)

      scaleY v = clamp 0 (hC-1) $ round ((ymax - v) / (ymax - ymin + eps) * fromIntegral (hC-1))

      emptyGrid = replicate hC (replicate wC (' ', Nothing))
      
      drawBox grid (idx, (_name, (minV, q1, median, q3, maxV))) =
        let xStart = idx * (boxWidth + spacing)
            xMid = xStart + boxWidth `div` 2
            xEnd = xStart + boxWidth - 1
            
            minRow = scaleY minV
            q1Row = scaleY q1
            medRow = scaleY median
            q3Row = scaleY q3
            maxRow = scaleY maxV
            
            col = pieColors !! (idx `mod` length pieColors)

            grid1 = drawVLine grid xMid minRow q1Row '│' (Just col)
            grid2 = drawVLine grid1 xMid q3Row maxRow '│' (Just col)
            
            grid3 = drawHLine grid2 xStart xEnd q1Row '─' (Just col)
            grid4 = drawHLine grid3 xStart xEnd q3Row '─' (Just col)
            grid5 = drawVLine grid4 xStart q1Row q3Row '│' (Just col)
            grid6 = drawVLine grid5 xEnd q1Row q3Row '│' (Just col)
            
            grid7 = drawHLine grid6 xStart xEnd medRow '═' (Just col)
            
            grid8 = setGridChar grid7 xMid minRow '┬' (Just col)
            grid9 = setGridChar grid8 xMid maxRow '┴' (Just col)
        in grid9
      
      finalGrid = List.foldl' drawBox emptyGrid (zip [0..] stats)
      
      ax = axisifyGrid cfg finalGrid (0, fromIntegral nBoxes) (ymin, ymax)
      legend = legendBlock (legendPos cfg) (leftMargin cfg + widthChars cfg)
                 [(name, Solid, pieColors !! (i `mod` length pieColors)) 
                  | (i, (name, _)) <- zip [0..] stats]
  in drawFrame cfg ax legend
  where
    drawVLine grid x y1 y2 ch col =
      let yStart = min y1 y2
          yEnd = max y1 y2
      in List.foldl' (\g y -> setGridChar g x y ch col) grid [yStart..yEnd]
    
    drawHLine grid x1 x2 y ch col =
      let xStart = min x1 x2
          xEnd = max x1 x2
      in List.foldl' (\g x -> setGridChar g x y ch col) grid [xStart..xEnd]
    
    setGridChar grid x y ch col =
      if y >= 0 && y < length grid && x >= 0 && x < gridWidth grid
      then take y grid <> [setAt (grid !! y) x (ch, col)] <> drop (y+1) grid
      else grid
      where setAt row i v = take i row <> [v] <> drop (i+1) row

heatmap :: [[Double]] -> Plot -> Text
heatmap matrix cfg =
  let rows = length matrix
      cols = gridWidth matrix

      allVals = concat matrix
      vmin = if null allVals then 0 else minimum' allVals
      vmax = if null allVals then 1 else maximum' allVals
      vrange = vmax - vmin

      intensityColors = 
        [ Blue, BrightBlue, Cyan, BrightCyan, Green, BrightGreen, 
          Yellow, BrightYellow, Magenta, BrightRed, Red
        ]
      
      colorForValue v =
        if vrange < eps 
        then Green
        else
          let norm = clamp 0 1 ((v - vmin) / vrange)
              idx = floor (norm * fromIntegral (length intensityColors - 1))
              idx' = clamp 0 (length intensityColors - 1) idx
          in intensityColors !! idx'

      plotW = widthChars cfg
      plotH = heightChars cfg
      
      displayGrid = 
        [ [ let 
                matrixRow = min (rows - 1) ((plotH - 1 - i) * rows `div` plotH)
                matrixCol = min (cols - 1) (j * cols `div` plotW)
                val = matrix !! matrixRow !! matrixCol
            in ('█', Just (colorForValue val))
          | j <- [0..plotW-1]]
        | i <- [0..plotH-1]]

      ax = axisifyGrid cfg displayGrid (0, fromIntegral cols - 1) (0, fromIntegral rows - 1)
      
      gradientLegend = (Text.pack $ printf "%.2f " vmin) <> 
                      Text.concat (fmap (\col -> paint col '█') intensityColors) <> 
                      (Text.pack $ printf " %.2f" vmax)
      
  in drawFrame cfg ax gradientLegend

stackedBars :: [(Text, [(Text, Double)])] -> Plot -> Text
stackedBars categories cfg =
  let wC = widthChars cfg
      hC = heightChars cfg

      seriesNames = case categories of
        []     -> []
        (c:_) -> map fst (snd c)
      
      totals = [sum (map snd series') | (_, series') <- categories]
      maxHeight = maximum (1e-12 : totals)
      
      nCats = length categories
      (base, extra) = if nCats == 0 then (0, 0) 
                      else (wC `div` nCats, wC - (wC `div` nCats) * nCats)
      widths = [base + (if i < extra then 1 else 0) | i <- [0..nCats-1]]
      
      cols = cycle paletteColors
      seriesColors = zip seriesNames cols
      
      
      makeBar (_, series') width =
        let cumHeights = scanl (+) 0 [v / maxHeight | (_, v) <- series']
            segments = zip3 (map fst series') cumHeights (drop 1 cumHeights)
            
            makeColumn :: [(Char, Maybe Color)]
            makeColumn = 
              [ let heightFromBottom = fromIntegral (hC - y) / fromIntegral hC
                    findSegment [] = (' ', Nothing)
                    findSegment ((name, bottom, top):rest) =
                      if heightFromBottom > bottom && heightFromBottom <= top
                      then ('█', lookup name seriesColors)
                      else findSegment rest
                in findSegment segments
              | y <- [0..hC-1]]
        in replicate width makeColumn
      
      gutterCol = replicate hC (' ', Nothing)
      allBars = zipWith makeBar categories widths
      columns = concat (List.intersperse [gutterCol] allBars)
      
      grid = [[col !! y | col <- columns] | y <- [0..hC-1]]
      
      ax :: Text
      ax = axisifyGrid cfg grid (0, fromIntegral (max 1 nCats)) (0, maxHeight)
      legend :: Text
      legend = legendBlock (legendPos cfg) (leftMargin cfg + 1 + 
                          (gridWidth grid))
                 [(name, Solid, col) | (name, col) <- seriesColors]
  in drawFrame cfg ax legend

gridWidth :: [[a]] -> Int
gridWidth []     = 0
gridWidth (x:_) = length x

-- | Min and max function for axis bounds which defaults to 0 and 1 when empty.
minimum', maximum' :: [Double] -> Double
minimum' [] = 0
minimum' xs = minimum xs
maximum' [] = 1
maximum' xs = maximum xs

-- AVL Tree we'll use as an array.
-- This improves upon the previous implementation that relies
-- on linked list for indexing and update (both O(n)) while keeping
-- the dependencies very light (wouldn't want to install all of containers
-- just to get an int map).
data Arr a 
  = E 
  | N Int Int (Arr a) a (Arr a)

size :: Arr a -> Int
size E               = 0
size (N sz _ _ _ _)  = sz

height :: Arr a -> Int
height E               = 0
height (N _ h _ _ _)   = h

mk :: Arr a -> a -> Arr a -> Arr a
mk l x r = N sz h l x r
  where
    sl = size l
    sr = size r
    hl = height l
    hr = height r
    sz = 1 + sl + sr
    h  = 1 + (if hl >= hr then hl else hr)

rotateL :: Arr a -> Arr a
rotateL (N _ _ l x (N _ _ rl y rr)) = mk (mk l x rl) y rr
rotateL _ = error "rotateL: malformed tree"

rotateR :: Arr a -> Arr a
rotateR (N _ _ (N _ _ ll y lr) x r) = mk ll y (mk lr x r)
rotateR _ = error "rotateR: malformed tree"

balance :: Arr a -> Arr a
balance t@(N _ _ l x r)
  | height l > height r + 1 =
      case l of
        N _ _ ll _ lr ->
          if height ll >= height lr
             then rotateR t
             else rotateR (mk (rotateL l) x r)
        _ -> t
  | height r > height l + 1 =
      case r of
        N _ _ rl _ rr ->
          if height rr >= height rl
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
      let sl = size l in
      if i < 0 || i >= 1 + sl + size r then error ("index out of bounds: " <> show i)
      else if i < sl then indexA l i
      else if i == sl then x
      else indexA r (i - sl - 1)

setA :: Arr a -> Int -> a -> Arr a
setA t i y =
  case t of
    E -> error ("index out of bounds when setting: " <> show i)
    N _ _ l x r ->
      let sl = size l in
      if i < 0 || i >= 1 + sl + size r then error ("index out of bounds: " <> show i)
      else if i < sl then balance (mk (setA l i y) x r)
      else if i == sl then mk l y r
      else balance (mk l x (setA r (i - sl - 1) y))

fromList :: [a] -> Arr a
fromList xs = fst (build (length xs) xs)
  where
    build :: Int -> [a] -> (Arr a, [a])
    build 0 ys = (E, ys)
    build n ys =
      let (l, ys1)   = build (n `div` 2) ys
          (x,ys2)      = case ys1 of
            []     -> error "IMPOSSIBLE"
            (v:vs) -> (v, vs)
          (r, ys3)   = build (n - n `div` 2 - 1) ys2
      in (mk l x r, ys3)
