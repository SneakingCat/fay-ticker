{-# LANGUAGE NoImplicitPrelude #-}
module Ticker (
  tickerInit
  ) where

import Prelude
import FFI
import JSAPI
import CanvasAPI
import Ref

-- | Init the ticker
tickerInit :: String -> Fay ()
tickerInit name = do
  -- Fetch canvas and context
  canvas <- getElementById name
  context <- getContext canvas "2d"
  
  -- Setting dimensions for the canvas
  setWidth canvas $ floor cwidth
  setHeight canvas $ floor cheight
  
  -- Initial rendering of the graph
  render context 0
  
  -- Activete the animation timer
  currTime <- newRef (1 :: Int)
  setInterval (animate context currTime) 1000
  
-- | Animate
animate :: Context -> Ref Int -> Fay ()
animate context currTime = do
  start <- readRef currTime
  render context start
  writeRef currTime (start+1)  

-- | Render the graph
render :: Context -> Int -> Fay ()
render context start = do
  -- Fill the graph with the background color
  setFillStyle context "#040404"
  fillRect context (0, 0) (cwidth, cheight)
  
  setStrokeStyle context "#358800"
  setLineWidth context 1
  renderHorizonalLines context
  setFont context "9px sans-serif"
  let dummyData = mkDummyData 60 start 100
  renderTimeMarks context dummyData
  setStrokeStyle context "red"
  renderDataPoints context dummyData

-- | Render horizonal lines to mark levels on the y-axis
renderHorizonalLines :: Context -> Fay ()
renderHorizonalLines context = do
  beginPath context
  forM_ [0,10 .. 100] -- Parameter driven?
    (\p ->
      dottedLine context (gleft, yOnGraph p) (gright, yOnGraph p) 10
    )
  stroke context
  
-- | Render marks on the x-axis
renderTimeMarks :: Context -> TimeSerie -> Fay ()
renderTimeMarks context (TimeSerie _ (_,timeItems)) = do
  beginPath context
  setTextBaseline context "middle"
  forM_ timeItems 
    (\(TimeItem x s) -> do
      let xpos = xOnGraph $ fromIntegral x
      dottedLine context (xpos, gtop) (xpos, gbottom) 10
      preservingMatrix context $ do
        translate context (xpos, tbottom)
        rotate context $ degToRad (-90)
        strokeText context (secsToString s) (0, 0)
    )
  stroke context

-- | Render the data points
renderDataPoints :: Context -> TimeSerie -> Fay ()
renderDataPoints context (TimeSerie mx (p:ps, _)) = do
  beginPath context
  -- First move the pen to the position of the first point
  initialMove p
  forM_ ps
    (\(DataItem xVal yVal) ->
      lineTo context (xOnGraph . fromIntegral $ xVal,
                      yOnGraph . norm $ yVal)
    )
  stroke context
  where 
    norm n = (n/mx) * 100
    initialMove (DataItem xVal yVal) = 
      moveTo context (xOnGraph . fromIntegral $ xVal,
                      yOnGraph . norm $ yVal)

-- | Map an x-axis value to graph position to draw it
yOnGraph :: Double -> Double
yOnGraph y = gbottom - gheight * (y/100)

-- | May an y-axis value to graph position to draw it
xOnGraph :: Double -> Double
xOnGraph x = gwidth * (x/60)

-- | Dimensions for the canvas
ctop, cbottom, cleft, cright, cwidth, cheight :: Double
ctop = 0; cbottom = 209
cleft = 0; cright = 499
cwidth = cright + 1; cheight = cbottom + 1

-- | Dimensions for the graph
gtop, gbottom, gleft, gright, gwidth, gheight :: Double
gtop = ctop + 10; gbottom = cbottom - 50
gleft = cleft; gright = cright;
gwidth = gright + 1; gheight = gbottom - gtop + 1

-- | Dimensions for the time stamp text
tbottom :: Double
tbottom = cbottom - 2

data TimeSerie = TimeSerie Double ([DataItem], [TimeItem])
               deriving Show
data DataItem  = DataItem Int Double
               deriving Show
data TimeItem  = TimeItem Int Int
               deriving Show

mkDummyData :: Int -> Int -> Double -> TimeSerie
mkDummyData num start mx =
  let
    ns = [0 .. num-1]
    d  = map mkItem ns
    t  = [mkTime n | n <- ns, isMod5 (n+start)]
  in
   TimeSerie mx (d, t)
  where
    mkItem n  = DataItem n $ mkValue (n+start)
    mkValue n = (1 + (sin $ fromIntegral n)) * (mx/2)
    mkTime n  = TimeItem n (n+start)
    isMod5 n  = n `mod` 5 == 0
    
-- | Convert a number of seconds to format "hh:mm:ss"
secsToString :: Int -> String
secsToString s = 
  let
    sec = s `mod` 60
    mns = (floor $ fromIntegral s/60) `mod` 60
    hrs = floor $ fromIntegral s/3600
  in
   (toStr hrs) ++ ":" ++ (toStr mns) ++ ":" ++ (toStr sec)
   where
     toStr n
       | n < 10    = "0" ++ (show n)
       | otherwise = show n
                     
-- | Convert degrees to radians
degToRad :: Double -> Double
degToRad deg = (pi/180)*deg