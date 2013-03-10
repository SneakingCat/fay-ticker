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
prop_startPointIsCorrect :: DottedLine -> Bool
prop_startPointIsCorrect (DottedLine start end seg ((point,_):_)) = 
  point == start

-- | The sign of the endpoint shall be the same for all points except
-- for the first point
prop_pointsShallHaveEqualSign :: DottedLine -> Bool
prop_pointsShallHaveEqualSign (DottedLine start end seg (p:ps)) =
  foldl (checkSign end) True ps

checkSign :: Point -> Bool -> (Point, Point) -> Bool
checkSign (x, y) b ((x', y'), (x'', y'')) =
  b && hasSameSign x x' x'' && hasSameSign y y' y''
  
hasSameSign x y z
  | x >= 0 && y >= 0 && z >= 0 = True
  | x < 0 && y < 0 && z < 0    = True
  | otherwise                  = False
