module Main (main) where

import TickerTimeSerie
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
  ]

-- Properties for the timestamp to string conversion
prop_timeIsEightChars :: Positive Int -> Bool
prop_timeIsEightChars (Positive n) = length (showTime n) == 8

prop_timeHasFormat :: Positive Int -> Bool
prop_timeHasFormat (Positive n) = 
  hasCorrectFormat $ showTime n
  where
    hasCorrectFormat [h1,h2,':',m1,m2,':',s1,s2] =
      (h1 >= '0' && h1 <= '9')
      && (h2 >= '0' && h2 <= '9')
      && (m1 >= '0' && m1 <= '5')
      && (m2 >= '0' && m2 <= '9')
      && (s1 >= '0' && s1 <= '5')
      && (s2 >= '0' && s2 <= '9')
    hasCorrectFormat _ = False

prop_timeHasSeconds :: Positive Int -> Bool
prop_timeHasSeconds (Positive n) =
  let
    n'   = wrapSeconds n
    s    = n' `mod` 60
    sstr = drop 6 $ showTime n
  in
   s == (read sstr)
   
prop_timeHasMinutes :: Positive Int -> Bool
prop_timeHasMinutes (Positive n) =
  let
    n'   = wrapSeconds n
    m    = (n' `div` 60) `mod` 60
    mstr = take 2 $ drop 3 $ showTime n
  in
   m == (read mstr)
   
prop_timeHasHours :: Positive Int -> Bool
prop_timeHasHours (Positive n) =
  let
    n'   = wrapSeconds n
    h    = n' `div` 3600
    hstr = take 2 $ showTime n
  in
   h == (read hstr)

wrapSeconds :: Int -> Int
wrapSeconds s = s `mod` (100 * 3600)

-- Properties for the dummy data generator
prop_timeSerieHasLength :: Positive Int    
                           -> Positive Double 
                           -> Property
prop_timeSerieHasLength (Positive s) (Positive m) =
  genSize (0, 100) $ (\n -> checkLength n $ mkSineTimeSerie n s m)
  where
    checkLength num (TimeSerie _ (timeSerie, _)) = (length timeSerie) == num
    
prop_timeSerieHasMaxValue :: Positive Int
                             -> Positive Double
                             -> Property
prop_timeSerieHasMaxValue (Positive s) (Positive m) =
  genSize (1, 100) $ (\n -> checkMaxValue m $ mkSineTimeSerie n s m)
  where
    checkMaxValue mv (TimeSerie mv' (timeSerie, _)) = 
      mv == mv' && (maxInTimeSerie mv timeSerie) <= mv
    maxInTimeSerie mv timeSerie = foldl findMax mv timeSerie
    findMax mv (DataItem _ mv') = if mv' > mv then mv' else mv
    
prop_timeSerieIsGteZero :: Positive Int
                             -> Positive Double
                             -> Property
prop_timeSerieIsGteZero (Positive s) (Positive m) =
  genSize (1, 100) $ (\n -> checkBounds (>= 0) $ mkSineTimeSerie n s m)
  
prop_timeSerieIsLteMax :: Positive Int
                          -> Positive Double
                          -> Property
prop_timeSerieIsLteMax (Positive s) (Positive m) =
  genSize (1, 100) $ (\n -> checkBounds (<= m) $ mkSineTimeSerie n s m)
  
checkBounds :: (Double -> Bool) -> TimeSerie -> Bool
checkBounds f (TimeSerie _ (timeSerie, _)) = 
  foldl (\b (DataItem _ v) -> (f v) && b) True timeSerie

genSize t = forAll (choose t)
  