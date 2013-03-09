{-# LANGUAGE NoImplicitPrelude #-}
module Ticker (
  tickerInit
  ) where

import Prelude
import FFI
import JSAPI
import CanvasAPI
import Ref
import TickerTimeSerie

-- | Mutable state for the ticker
data State = State {
  drawingContext :: Context
  , graphButtons :: [Element]
  , colorButtons :: [Element]
  , dataRenderer :: DataRenderer
  , graphColors  :: Colors
  , timeSerie    :: TimeSerie
  , startTime    :: Int
  }
             
-- | Color descriptions for the ticker graph
data Colors = Colors {
  background :: String
  , grid     :: String
  , plot     :: String
  }             

-- | Function signature for data renderers
type DataRenderer = Context -> TimeSerie -> Fay ()

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
  graphButtons' <- getElementsById ["CurveButton", "BarButton"]
  -- Make the "curve mode" the initial mode
  selectFirst graphButtons'
  
  -- Fetch the color scheme button references
  colorButtons' <- getElementsById ["BlackButton", "BlueButton"]
  -- Make the "black scheme" the initial scheme
  selectFirst colorButtons'
  
  -- Create the global state
  state <- newRef State {drawingContext=context
                        , graphButtons =graphButtons'
                        , colorButtons =colorButtons'
                        , dataRenderer =renderDataPlotAsCurves
                        , graphColors  =blackColorScheme
                        , timeSerie    =mkSineTimeSerie 60 0 100
                        , startTime    =1}
           
  -- Install event handlers
  addButtonListeners graphButtons' handleGraphButton 
    [renderDataPlotAsCurves, renderDataPlotAsBars] state
  addButtonListeners colorButtons' handleColorButton
    [blackColorScheme, blueColorScheme] state

  -- Initial rendering of the graph
  render state
  
  -- Activete the animation timer
  setInterval (animate state) 1000

-- | Render the graph
render :: Ref State -> Fay ()
render state = do
  state' <- readRef state
  let context  = drawingContext state'
  let colors   = graphColors state'
  let renderer = dataRenderer state'
  let tserie   = timeSerie state'
      
  -- Fill the graph with the background color
  setFillStyle context $ background colors
  fillRect context (0, 0) (cwidth, cheight)
  
  setStrokeStyle context $ grid colors
  renderHorizonalLines context
  setFont context "9px sans-serif"
  renderTimeMarks context tserie
  setStrokeStyle context $ plot colors

  -- The data renderer is taken from the state
  renderer context tserie

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
        strokeText context (showTime s) (0, 0)
    )
  stroke context
  where
    degToRad deg = (pi/180)*deg

-- | Render the data plot as curves
renderDataPlotAsCurves :: DataRenderer
renderDataPlotAsCurves context (TimeSerie mx (p:ps, _)) = do
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
      
-- | Render the data plot as bars
renderDataPlotAsBars :: DataRenderer
renderDataPlotAsBars context (TimeSerie mx (ps,_)) = do
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
    
-- | Generate a black color scheme
blackColorScheme :: Colors
blackColorScheme = Colors {background="#040404"
                          , grid     ="#358800"
                          , plot     ="red"}

-- | Generate a blue color scheme
blueColorScheme :: Colors
blueColorScheme = Colors {background="#F2F7FE"
                         , grid     ="#7B899B"
                         , plot     ="#7B899B"}          
          
-- | Register a button callback
addButtonListeners :: [Element]
                      -> (Ref State -> i -> Element -> Event -> Fay Bool)
                      -> [i]
                      -> Ref State
                      -> Fay ()
addButtonListeners buttons cb items state =
  forM_ (zip buttons items) (\(b, i) ->
                              addEventListener b "click" (cb state i b))

selectFirst :: [Element] -> Fay ()
selectFirst [] = return ()
selectFirst (x:_) = setClassName "Selected" x

-- | Handle the event of user clicking the graph buttons
handleGraphButton :: Ref State
                     -> DataRenderer 
                     -> Element
                     -> Event
                     -> Fay Bool  
handleGraphButton state renderer me _ = do
  state' <- readRef state  
  mapM_ (setClassName "") $ graphButtons state'
  writeRef state $ state' {dataRenderer=renderer}
  setClassName "Selected" me
  render state
  return False
  
-- | Handle the event of user clicking the color buttons
handleColorButton :: Ref State
                     -> Colors
                     -> Element
                     -> Event     
                     -> Fay Bool
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
  writeRef state $ state' {timeSerie=mkSineTimeSerie 60 (startTime state') 100
                          , startTime=startTime state'+1}
  render state
