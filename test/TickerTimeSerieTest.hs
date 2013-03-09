module TickerTimeSerieTest where

import TickerTimeSerie
import Test.QuickCheck

-- Properties for the timestamp to string conversion
prop_timeIsEightChars :: Positive Int -> Bool
prop_timeIsEightChars (Positive n) = length (showTime n) == 8

prop_timeHasFormat :: Positive Int -> Bool
prop_timeHasFormat (Positive n) = 
  hasCorrectFormat $ showTime n
  where
    hasCorrectFormat [h1,h2,':',m1,m2,':',s1,s2] =
      inRange h1 ('0', '9')
      && inRange h2 ('0', '9')
      && inRange m1 ('0', '5')
      && inRange m2 ('0', '9')
      && inRange s1 ('0', '5')
      && inRange s2 ('0', '9')
    hasCorrectFormat _ = False
    inRange d (l,h) = d >= l && d <= h

prop_timeHasSeconds :: Positive Int -> Bool
prop_timeHasSeconds (Positive n) =
  let
    n'   = wrapSeconds n
    s    = n' `mod` 60
    sstr = drop 6 $ showTime n
  in
   s == read sstr
   
prop_timeHasMinutes :: Positive Int -> Bool
prop_timeHasMinutes (Positive n) =
  let
    n'   = wrapSeconds n
    m    = (n' `div` 60) `mod` 60
    mstr = take 2 $ drop 3 $ showTime n
  in
   m == read mstr
   
prop_timeHasHours :: Positive Int -> Bool
prop_timeHasHours (Positive n) =
  let
    n'   = wrapSeconds n
    h    = n' `div` 3600
    hstr = take 2 $ showTime n
  in
   h == read hstr

wrapSeconds :: Int -> Int
wrapSeconds s = s `mod` (100 * 3600)

-- Properties for the dummy data generator
prop_timeSerieHasLength :: Positive Int    
                           -> Positive Double 
                           -> Property
prop_timeSerieHasLength (Positive s) (Positive m) =
  genSize (0, 100) (\n -> checkLength n $ mkSineTimeSerie n s m)
  where
    checkLength num (TimeSerie _ (timeSerie, _)) = length timeSerie == num
    
prop_timeSerieHasMaxValue :: Positive Int
                             -> Positive Double
                             -> Property
prop_timeSerieHasMaxValue (Positive s) (Positive m) =
  genSize (1, 100) (\n -> checkMaxValue m $ mkSineTimeSerie n s m)
  where
    checkMaxValue mv (TimeSerie mv' (timeSerie, _)) = 
      mv == mv' && maxInTimeSerie mv timeSerie <= mv
    maxInTimeSerie              = foldl findMax
    findMax mv (DataItem _ mv') = if mv' > mv then mv' else mv
    
prop_timeSerieIsGteZero :: Positive Int
                             -> Positive Double
                             -> Property
prop_timeSerieIsGteZero (Positive s) (Positive m) =
  genSize (1, 100) (\n -> checkBounds (>= 0) $ mkSineTimeSerie n s m)
  
prop_timeSerieIsLteMax :: Positive Int
                          -> Positive Double
                          -> Property
prop_timeSerieIsLteMax (Positive s) (Positive m) =
  genSize (1, 100) (\n -> checkBounds (<= m) $ mkSineTimeSerie n s m)
  
checkBounds :: (Double -> Bool) -> TimeSerie -> Bool
checkBounds f (TimeSerie _ (timeSerie, _)) = 
  foldl (\b (DataItem _ v) -> f v && b) True timeSerie

genSize t = forAll (choose t)
  