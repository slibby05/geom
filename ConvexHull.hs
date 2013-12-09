module ConvexHull (convexHull) where

import Vec
import Mat
import Data.List

convexHull :: (Show a, Eq a, Ord a, Num a) => [Vec2 a] -> [Vec2 a]
convexHull ps = (hullSide turnLeft (sort ps) []) ++ (hullSide turnLeft (reverse $ sort ps) [])
 where
  turnLeft p q r = turnSide p q r >= 0

hullSide :: (Vec2 a -> Vec2 a -> Vec2 a -> Bool) -> [Vec2 a] -> [Vec2 a] -> [Vec2 a]
hullSide _ [] hull = hull
hullSide turn (p:ps) [] = hullSide turn ps [p]
hullSide turn (p:ps) [t] = hullSide turn ps [p,t]
hullSide turn (p:ps) (t:b:hull)
 | turn b t p = hullSide turn ps (p:t:b:hull)
 | otherwise  = hullSide turn (p:ps) (b:hull)

turnSide :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a -> a
turnSide p q r = det matrix
 where
  matrix = Mat3 (Vec3 1 1 1) (Vec3 (p!0) (q!0) (r!0)) (Vec3 (p!1) (q!1) (r!1))

