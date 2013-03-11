module GeometryTest where

import Test.QuickCheck
import Control.Monad
import Geometry

-- | A data structure which both represents the "point list" as well
-- as the randomly selected parameters used to generate the list
data DottedLine = DottedLine Point Point Double [(Point, Point)]
                deriving Show

instance Arbitrary DottedLine where
  arbitrary = do
    let start = (0,0)
    end <- liftM2 (,) (choose (-10,10)) (choose (-10,10))
    seg <- choose (0.1,0.5)
    return $ DottedLine start end seg $ genDottedLine start end seg

-- | The start point in the list shall be exact as specified when
-- generating the list
prop_startPtIsCorrect :: DottedLine -> Bool
prop_startPtIsCorrect (DottedLine start end seg ((point,_):_)) = 
  point == start

-- | The sign of the endpoint shall be the same for all points except
-- for the first point
prop_ptsShallHaveEqualSign :: DottedLine -> Bool
prop_ptsShallHaveEqualSign (DottedLine start end seg (p:ps)) =
  foldl (checkSign end) True ps
  
-- | Points shall go in half segment long steps
prop_ptsShallGoInEqualSteps :: DottedLine -> Bool
prop_ptsShallGoInEqualSteps (DottedLine start end seg ps) =
  foldl (checkLength (seg/2)) True ps

checkSign :: Point -> Bool -> (Point, Point) -> Bool
checkSign (x, y) b ((x', y'), (x'', y'')) =
  b && hasSameSign x x' x'' && hasSameSign y y' y''
  where
    hasSameSign x y z
      | x >= 0 && y >= 0 && z >= 0 = True
      | x < 0 && y < 0 && z < 0    = True
      | otherwise                  = False                                     
   
checkLength :: Double -> Bool -> (Point, Point) -> Bool
checkLength seg b (p1,p2) = b && fuzzyEqual seg (lineLength p1 p2)

lineLength :: Point -> Point -> Double
lineLength (x,y) (x',y') =
  let
    dx = x'-x
    dy = y'-y
  in
   sqrt $ dx^2+dy^2

fuzzyEqual :: (Ord a, Floating a) => a -> a -> Bool
fuzzyEqual a b = abs (a - b) < 0.000001
