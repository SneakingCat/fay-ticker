{-# LANGUAGE CPP #-}
module TickerTimeSerie (
  TimeSerie (..)
  , DataItem (..)
  , TimeItem (..)
  , mkSineTimeSerie
  , showTime
  ) where

#ifdef FAY
import Prelude
#endif

-- | Data structure for the time serie data. The time serie will be
-- rendered "as is", it's the server's responsibility to ensure the
-- format of the time serie
data TimeSerie = TimeSerie Double ([DataItem], [TimeItem])
               deriving Show
data DataItem  = DataItem Int Double
               deriving Show
data TimeItem  = TimeItem Int Int
               deriving Show
                        
-- | Make a dummy - sine wave - time serie
mkSineTimeSerie :: Int -> Int -> Double -> TimeSerie
mkSineTimeSerie num start mx =
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
showTime :: Int -> String
showTime s =
  let
    s'   = s `mod` wrapPeriod
    sec  = s' `mod` secPerMin
    min' = (s' `div` secPerMin) `mod` minPerHour
    hrs  = s' `div` secPerHour
  in
   (asStr hrs) ++ ":" ++ (asStr min') ++ ":" ++ (asStr sec)
   where
     asStr n
       | n < 10    = "0" ++ (show n)
       | otherwise = show n
     wrapPeriod = 99 * secPerHour
     secPerMin  = 60
     minPerHour = 60
     secPerHour = secPerMin * minPerHour