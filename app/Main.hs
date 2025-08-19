module Main where

import Control.Monad
import System.Random.Stateful

import Granite

main :: IO ()
main = do
  -- scatter
  g <- newIOGenM =<< newStdGen
  let range = (0 :: Double, 1 :: Double)
  ptsA_x <- replicateM 600 (uniformRM range g)
  ptsA_y <- replicateM 600 (uniformRM range g)
  ptsB_x <- replicateM 600 (uniformRM range g)
  ptsB_y <- replicateM 600 (uniformRM range g)
  putStrLn =<< scatter "Random points" [series "A" (zip ptsA_x ptsA_y), series "B" (zip ptsB_x ptsB_y)] defPlot{widthChars=68,heightChars=22}

  -- histogram
  heights <- replicateM 5000 (uniformRM (160 :: Double, 190 :: Double) g)
  putStrLn =<< histogram "Heights (cm)" (bins 30 155 195) heights defPlot{widthChars=68,heightChars=18,legendPos=LegendBottom}

  -- bars
  putStrLn =<< bars "Sales" [("Q1",12),("Q2",18),("Q3",9),("Q4",15)] defPlot

  -- pie
  putStrLn =<< pie "Share" [("Alpha",0.35),("Beta",0.25),("Gamma",0.20),("Delta",0.20)]
                         defPlot{widthChars=46,heightChars=18,legendPos=LegendRight}
