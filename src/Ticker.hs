{-# LANGUAGE NoImplicitPrelude #-}
module Ticker (
  tickerInit
  ) where

import Prelude
import FFI
import JSApi

-- | Init the ticker
tickerInit :: Fay ()
tickerInit = do
  -- Fetch canvas and context
--  canvas <- getElementById "canvas"
--  context <- getContext canvas "2d"
  
  -- Setting dimensions for the canvas
--  setWidth canvas $ floor cwidth
--  setHeight canvas $ floor cheight
  
  -- Initial rendering of the graph
--  render context 0
  
  -- Activete the animation timer
--  currTime <- newRef (1 :: Int)
--  setInterval (animate context currTime) 1000
    
  alert "Hej från Ticker"