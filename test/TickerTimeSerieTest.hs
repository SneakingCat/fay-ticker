module Main (main) where

import TickerTimeSerie
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Test group 1" [
     testProperty "Is eight chars" prop_timeIsEightChars
     , testProperty "Has correct format" prop_timeHasCorrectFormat
     , testProperty "Has correct seconds" prop_timeHasCorrectSeconds
     , testProperty "Has correct minutes" prop_timeHasCorrectMinutes
     , testProperty "Has correct hours" prop_timeHasCorrectHours
     ]
  ]

prop_timeIsEightChars :: Positive Int -> Bool
prop_timeIsEightChars (Positive n) = length (mkTime n) == 8

prop_timeHasCorrectFormat :: Positive Int -> Bool
prop_timeHasCorrectFormat (Positive n) = 
  hasCorrectFormat $ mkTime $ wrapSeconds n
  where
    hasCorrectFormat [h1,h2,':',m1,m2,':',s1,s2] =
      (h1 >= '0' && h1 <= '9')
      && (h2 >= '0' && h2 <= '9')
      && (m1 >= '0' && m1 <= '5')
      && (m2 >= '0' && m2 <= '9')
      && (s1 >= '0' && s1 <= '5')
      && (s2 >= '0' && s2 <= '9')
    hasCorrectFormat _ = False

prop_timeHasCorrectSeconds :: Positive Int -> Bool
prop_timeHasCorrectSeconds (Positive n) =
  let
    n'   = wrapSeconds n
    s    = n' `mod` 60
    sstr = drop 6 $ mkTime n'
  in
   s == (read sstr)
   
prop_timeHasCorrectMinutes :: Positive Int -> Bool
prop_timeHasCorrectMinutes (Positive n) =
  let
    n'   = wrapSeconds n
    m    = (n' `div` 60) `mod` 60
    mstr = take 2 $ drop 3 $ mkTime n'
  in
   m == (read mstr)
   
prop_timeHasCorrectHours :: Positive Int -> Bool
prop_timeHasCorrectHours (Positive n) =
  let
    n'   = wrapSeconds n
    h    = n' `div` 3600
    hstr = take 2 $ mkTime $ wrapSeconds n'
  in
   h == (read hstr)

wrapSeconds :: Int -> Int
wrapSeconds s = s `mod` (99 * 3600)

mkTime :: Int -> String
mkTime s =
  let
    s'   = s `mod` (99 * 3600)
    sec  = s' `mod` 60
    min' = (s' `div` 60) `mod` 60
    hrs  = s' `div` 3600
  in
   (asStr hrs) ++ ":" ++ (asStr min') ++ ":" ++ (asStr sec)
   where
     asStr n
       | n < 10    = "0" ++ (show n)
       | otherwise = show n