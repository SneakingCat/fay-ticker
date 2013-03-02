{-# LANGUAGE NoImplicitPrelude #-}
module TickerApp (main) where

import Prelude
import FFI
import JSAPI
import Ticker

main :: Fay ()
main = 
  addWindowEventListener "load" handleLoad
  
handleLoad :: Event -> Fay Bool
handleLoad _ = do
  tickerInit "canvas"
  return False
