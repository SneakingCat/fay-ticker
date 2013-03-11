module TickerTimeSerieTest where

import TickerTimeSerie
import Test.QuickCheck

instance Arbitrary TimeSerie where
  arbitrary = do
    n <- choose (1, 100)
    s <- choose (0, 1000000)
    m <- choose (1, 1000000)
    return $ mkSineTimeSerie n s m

-- | Check that all generated values are less or equal to the max value
prop_timeSerieIsLteMax :: TimeSerie -> Bool
prop_timeSerieIsLteMax (TimeSerie mv (dataItems, timeItems)) =
  foldl (checkBounds (<= mv)) True dataItems
  
-- | Check that all generated values are greater or equal to zero
prop_timeSerieIsGteZero :: TimeSerie -> Bool
prop_timeSerieIsGteZero (TimeSerie mv (dataItems, timeItems)) =
  foldl (checkBounds (>= 0)) True dataItems

-- | Check that the data items are indexed 0 -> num data items - 1
prop_timeSerieIsIndexed :: TimeSerie -> Bool
prop_timeSerieIsIndexed (TimeSerie mv (dataItems, timeItems)) =
  let indices = map extractIndices dataItems
  in  indices == [0..(length indices)-1]
  where
    extractIndices (DataItem i v) = i

-- | Check that the time stamps (if any) are increasing in steps of five
prop_timeIncreasesByFive :: TimeSerie -> Bool
prop_timeIncreasesByFive (TimeSerie mv (dataItems, timeItems)) =
  let times = map extractTimes timeItems
  in  times == byFiveIncreasingList times
  where
    extractTimes (TimeItem i t) = t

-- | Check that the time stamps' (if any) indices are increasing in
-- steps of five
prop_timeIndIncreasesByFive :: TimeSerie -> Bool
prop_timeIndIncreasesByFive (TimeSerie mv (dataItems, timeItems)) =
  let indices = map extractIndices timeItems
  in  indices == byFiveIncreasingList indices
  where
    extractIndices (TimeItem i t) = i

-- | Helper function to check the bounds of a DataItem
checkBounds :: (Double -> Bool) -> Bool -> DataItem -> Bool
checkBounds f b (DataItem i v) = b && f v

-- | Helper function to generate a list which content increase by five
-- - given a list which content it shall try to copy (without copying
-- values)
byFiveIncreasingList :: [Int] -> [Int]
byFiveIncreasingList [] = []
byFiveIncreasingList l  =
  let s = head l
      e = s + (5 * length l)
  in [s,(s+5)..e-1]   

-- | Check that time stamp strings always are of length 9
prop_timeIsEightChars :: Positive Int -> Bool
prop_timeIsEightChars (Positive n) = length (showTime n) == 8

-- | Check that the time stamps strings has the format "hh:mm:ss"
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

-- | Check that the "ss" part is correct
prop_timeHasSeconds :: Positive Int -> Bool
prop_timeHasSeconds (Positive n) =
  let n'   = wrapSeconds n
      s    = n' `mod` 60
      sstr = drop 6 $ showTime n
  in  s == read sstr

-- | Check that the "mm" part is correct
prop_timeHasMinutes :: Positive Int -> Bool
prop_timeHasMinutes (Positive n) =
  let n'   = wrapSeconds n
      m    = (n' `div` 60) `mod` 60
      mstr = take 2 $ drop 3 $ showTime n
  in  m == read mstr
   
-- | Check that the "hh" part is correct
prop_timeHasHours :: Positive Int -> Bool
prop_timeHasHours (Positive n) =
  let n'   = wrapSeconds n
      h    = n' `div` 3600
      hstr = take 2 $ showTime n
  in  h == read hstr

wrapSeconds :: Int -> Int
wrapSeconds s = s `mod` (100 * 3600)
