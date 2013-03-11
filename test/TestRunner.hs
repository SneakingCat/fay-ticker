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
     testProperty   "Time serie is <= max" prop_timeSerieIsLteMax
     , testProperty "Time serie is >= zero" prop_timeSerieIsGteZero
     , testProperty "Time serie is indexed" prop_timeSerieIsIndexed
     , testProperty "Time stamps inc. by 5" prop_timeIncreasesByFive
     , testProperty "Time stamp indices inc. by 5" prop_timeIndIncreasesByFive
     , testProperty "Time has eight chars" prop_timeIsEightChars
     , testProperty "Time has correct format" prop_timeHasFormat
     , testProperty "Time has correct seconds" prop_timeHasSeconds
     , testProperty "Time has correct minutes" prop_timeHasMinutes
     , testProperty "Time has correct hours" prop_timeHasHours
     ]
  , testGroup "Geometry Tests" [
     testProperty   "Starting point is correct" prop_startPtIsCorrect
     , testProperty "Points shall have eq. sign" prop_ptsShallHaveEqualSign
     , testProperty "Points shall go in eq. steps" prop_ptsShallGoInEqualSteps
    ]
  ]