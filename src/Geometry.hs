{-# LANGUAGE CPP #-}
module Geometry (
  Point
  , genDottedLine
  ) where

#ifdef FAY
import Prelude
#endif

type Point = (Double, Double)

-- | Generate a list of points which can be used for drawing a dotted
-- line using sequences of moveTo and lineTo
genDottedLine :: Point -> Point -> Double -> [(Point, Point)]
genDottedLine (sx,sy) (ex,ey) seg =
  let dx    = ex-sx
      dy    = ey-sy
      l     = len dx dy
      xStep = dx/l
      yStep = dy/l
      half  = seg/2
  in
   map (\n -> (
           (sx + n * xStep, sy + n * yStep),
           (sx + (n+half) * xStep, sy + (n+half) * yStep)
           )) [0, seg .. l]
   where
     len :: Double -> Double -> Double
     len dx dy = sqrt $ (dx^2) + (dy^2)