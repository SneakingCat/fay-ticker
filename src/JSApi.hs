{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
module JSApi (
  Event
  , addWindowEventListener
  , getElementById
  , setInterval
  , alert
  ) where

import Prelude
import FFI

class Foreign a

data Event
instance Foreign Event

data Element
instance Foreign Element

-- | Add a window event listener for the given event
addWindowEventListener :: String -> (Event -> Fay Bool) -> Fay ()
addWindowEventListener = ffi "window['addEventListener'](%1,%2,false)"

-- | Get an element from the document
getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

-- | Set a timer
setInterval :: Fay () -> Double -> Fay ()
setInterval = ffi "window['setInterval'](%1,%2)"

-- | Popup an alert window with the given input as message
alert :: String -> Fay ()
alert = ffi "window['alert'](%1)"