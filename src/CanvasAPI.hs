{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
module CanvasAPI (
  Context
  , Point
  , Dim
  , getContext
  , setHeight
  , setWidth
  , setFillStyle
  , setStrokeStyle
  , setLineWidth
  , beginPath
  , endPath
  , moveTo
  , lineTo
  , stroke
  , fillRect
  , strokeText
  , setFont
  , setTextAlignment
  , setTextBaseline
  , save
  , restore
  , rotate
  , translate
  , dottedLine
  , preservingMatrix
  ) where

import Prelude
import FFI
import JSAPI

data Context
instance Foreign Context

type Point = (Double, Double)
type Dim = (Double, Double)

-- | Get the given context from the canvas
getContext :: Element -> String -> Fay Context
getContext = ffi "%1['getContext'](%2)"

-- | Set the height attribute for the canvas
setHeight :: Element -> Int -> Fay ()
setHeight = ffi "%1['height']=%2"

-- | Set the width attribute for the canvas
setWidth :: Element -> Int -> Fay ()
setWidth = ffi "%1['width']=%2"

-- | Set the fill color attribute
setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

-- | Set the stroke color attribute
setStrokeStyle :: Context -> String -> Fay ()
setStrokeStyle = ffi "%1['strokeStyle']=%2"

-- | Set the line width attribute
setLineWidth :: Context -> Double -> Fay ()
setLineWidth = ffi "%1['lineWidth']=%2"

-- | Begin a path
beginPath :: Context -> Fay ()
beginPath = ffi "%1['beginPath']()"

-- | End a path
endPath :: Context -> Fay ()
endPath = ffi "%1['endPath']()"

-- | Move to
moveTo :: Context -> Point -> Fay ()
moveTo context (x, y) = moveTo' context x y

moveTo' :: Context -> Double -> Double -> Fay ()
moveTo' = ffi "%1['moveTo'](%2,%3)"

-- | Line to
lineTo :: Context -> Point -> Fay ()
lineTo context (x, y) = lineTo' context x y

lineTo' :: Context -> Double -> Double -> Fay ()
lineTo' = ffi "%1['lineTo'](%2,%3)"

-- | Stroke
stroke :: Context -> Fay ()
stroke = ffi "%1['stroke']()"

-- | Fill the specified rectangle
fillRect :: Context -> Point -> Dim -> Fay ()
fillRect context (x, y) (w, h) = fillRect' context x y w h

fillRect' :: Context -> Double -> Double -> Double -> Double -> Fay ()
fillRect' = ffi "%1['fillRect'](%2,%3,%4,%5)"

-- | Stroke the text at the specified location
strokeText :: Context -> String -> Point -> Fay ()
strokeText context str (x, y) = strokeText' context str x y

strokeText' :: Context -> String -> Double -> Double -> Fay ()
strokeText' = ffi "%1['strokeText'](%2,%3,%4)"

-- | Set the font attribute
setFont :: Context -> String -> Fay ()
setFont = ffi "%1['font']=%2"

-- | Set the text alignment attribute
setTextAlignment :: Context -> String -> Fay ()
setTextAlignment = ffi "%1['textAlignment']=%2"

-- | Set the text baseline attribute
setTextBaseline :: Context -> String -> Fay ()
setTextBaseline = ffi "%1['textBaseline']=%2"

-- | Preserve the transformation matrix
save :: Context -> Fay ()
save = ffi "%1['save']()"

-- | Restore the transformation matrix
restore :: Context -> Fay ()
restore = ffi "%1['restore']()"

-- | Rotate the objecs to be drawn
rotate :: Context -> Double -> Fay ()
rotate = ffi "%1['rotate'](%2)"

-- | Translate the objects to be drawn
translate :: Context -> Point -> Fay ()
translate context (x, y) = translate' context x y

translate' :: Context -> Double -> Double -> Fay ()
translate' = ffi "%1['translate'](%2,%3)"

-- | Draw a dotted line between the specified points, by using the
-- given segment length
dottedLine :: Context -> Point -> Point -> Double -> Fay ()
dottedLine context start end seg =
  forM_ (genDottedLine start end seg) 
    (\(p1, p2) -> do
        moveTo context p1
        lineTo context p2)

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
     
-- | Preserving the transformation like in hOpenGL
preservingMatrix :: Context -> Fay a -> Fay ()
preservingMatrix context actions = do
  save context
  _ <- actions
  restore context