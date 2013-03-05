{-# LANGUAGE NoImplicitPrelude #-}
module Ticker (
  tickerInit
  ) where

import Prelude
import FFI
import JSAPI
import CanvasAPI
import Ref

data State = State {
  drawingContext :: Context
  , graphButtons :: [Element]
  , colorButtons :: [Element]
  , dataRenderer :: DataRenderer
  , graphColors  :: Colors
  , startTime    :: Int
  }
             
data Colors = Colors {
  background :: String
  , grid     :: String
  , plot     :: String
  }

-- | Init the ticker
tickerInit :: String -> Fay ()
tickerInit name = do
  -- Fetch canvas and canvas context
  canvas  <- getElementById name
  context <- getContext canvas "2d"
  
  -- Setting dimensions for the canvas
  setWidth canvas $ floor cwidth
  setHeight canvas $ floor cheight
  
  -- Fetch the graph button references
  curveButton <- getElementById "CurveButton"
  barButton   <- getElementById "BarButton"
  
  -- Make the "curve mode" the initial mode
  setClassName "Selected" curveButton
  
  -- Fetch the color scheme button references
  blackButton <- getElementById "BlackButton"
  blueButton  <- getElementById "BlueButton"
  
  -- Make the "black scheme" the initial scheme
  setClassName "Selected" blackButton
  
  -- Create the global state
  state <- newRef $ State {drawingContext=context
                          , graphButtons=[curveButton,barButton]
                          , colorButtons=[blackButton,blueButton]
                          , dataRenderer=renderDataPointsCurve
                          , graphColors=blackColorScheme
                          , startTime=0}
           
  -- Install event handlers
  addEventListener curveButton "click" 
    (handleGraphButton state renderDataPointsCurve curveButton)
  addEventListener barButton "click" 
    (handleGraphButton state renderDataPointsBar barButton)
  addEventListener blackButton "click"
    (handleColorButton state blackColorScheme blackButton)
  addEventListener blueButton "click"
    (handleColorButton state blueColorScheme blueButton)

  -- Initial rendering of the graph
  render state
  
  -- Activete the animation timer
  setInterval (animate state) 1000
  
blackColorScheme :: Colors
blackColorScheme = Colors {background="#040404"
                          , grid     ="#358800"
                          , plot     ="red"}
                   
blueColorScheme :: Colors
blueColorScheme = Colors {background="#F2F7FE"
                         , grid     ="#7B899B"
                         , plot     ="#7B899B"}

handleGraphButton :: Ref State    -> 
                     DataRenderer -> 
                     Element      -> 
                     Event        -> 
                     Fay Bool  
handleGraphButton state renderer me _ = do
  state' <- readRef state  
  mapM_ (setClassName "") $ graphButtons state'
  writeRef state $ state' {dataRenderer=renderer}
  setClassName "Selected" me
  render state
  return False
  
handleColorButton :: Ref State -> 
                     Colors    -> 
                     Element   -> 
                     Event     -> 
                     Fay Bool
handleColorButton state colors me _ = do
  state' <- readRef state
  mapM_ (setClassName "") $ colorButtons state'
  writeRef state $ state' {graphColors=colors}
  setClassName "Selected" me
  render state
  return False
  
-- | React upon timer and drive the animation
animate :: Ref State -> Fay ()
animate state = do
  state' <- readRef state
  writeRef state $ state' {startTime=startTime state'+1}
  render state

-- | Render the graph
render :: Ref State -> Fay ()
render state = do
  state' <- readRef state
  let context = drawingContext state'
  let start   = startTime state'
  let colors  = graphColors state'
      
  -- Fill the graph with the background color
  setFillStyle context $ background colors
  fillRect context (0, 0) (cwidth, cheight)
  
  setStrokeStyle context $ grid colors
  renderHorizonalLines context
  setFont context "9px sans-serif"
  let dummyData = mkDummyData 60 start 100
  renderTimeMarks context dummyData
  setStrokeStyle context $ plot colors

  -- The data renderer is taken from the state
  let renderer = dataRenderer state'
  renderer context dummyData

-- | Render horizonal lines to mark levels on the y-axis
renderHorizonalLines :: Context -> Fay ()
renderHorizonalLines context = do
  setLineWidth context 1
  beginPath context
  forM_ [0,10 .. 100] -- Parameter driven?
    (\p ->
      dottedLine context (gleft, yOnGraph p) (gright, yOnGraph p) 10
    )
  stroke context
  
-- | Render marks on the x-axis
renderTimeMarks :: Context -> TimeSerie -> Fay ()
renderTimeMarks context (TimeSerie _ (_,timeItems)) = do
  setLineWidth context 1
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

type DataRenderer = Context -> TimeSerie -> Fay ()

-- | Render the data points as a curve
renderDataPointsCurve :: DataRenderer
renderDataPointsCurve context (TimeSerie mx (p:ps, _)) = do
  setLineWidth context 1.5
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
      
-- | Render the data points as bars
renderDataPointsBar :: DataRenderer
renderDataPointsBar context (TimeSerie mx (ps,_)) = do
  setLineWidth context 5
  beginPath context
  forM_ ps
    (\(DataItem xVal yVal) -> do
      moveTo context (xOnGraph . fromIntegral $ xVal,
                      yOnGraph . norm $ yVal)
      lineTo context (xOnGraph . fromIntegral $ xVal,
                      yOnGraph 0)
    )
  stroke context
  where 
    norm n = (n/mx) * 100
      
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
    mkValue n = (1 + (sin $ freq * rad * fromIntegral n)) * (mx/2)
    mkTime n  = TimeItem n (n+start)
    isMod5 n  = n `mod` 5 == 0
    rad       = (2*pi)/(fromIntegral num)
    freq      = 2
    
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