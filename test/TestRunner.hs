module Main (main) where

import TickerTimeSerieTest
import GeometryTest
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "TimeSerie Tests" [
     testProperty   "Time has eight chars" prop_timeIsEightChars
     , testProperty "Time has correct format" prop_timeHasFormat
     , testProperty "Time has correct seconds" prop_timeHasSeconds
     , testProperty "Time has correct minutes" prop_timeHasMinutes
     , testProperty "Time has correct hours" prop_timeHasHours
     , testProperty "TimeSerie has correct length" prop_timeSerieHasLength
     , testProperty "TimeSerie has correct max value" prop_timeSerieHasMaxValue
     , testProperty "TimeSerie is >= zero" prop_timeSerieIsGteZero
     , testProperty "TimeSerie is <= max" prop_timeSerieIsLteMax
     ]
  , testGroup "Geometry Tests" [
     testProperty   "Starting point is correct" prop_startPtIsCorrect
     , testProperty "Points shall have eq. sign" prop_ptsShallHaveEqualSign
     , testProperty "Points shall go in eq. steps" prop_ptsShallGoInEqualSteps
    ]
  ]