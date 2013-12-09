{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Mat 
( Matrix(..),
  Mat2(..),
  Mat3(..),
  rotx,roty,rotz
) where

import Vec

class Vector v => Matrix m v | m -> v where
  row :: m a -> Int -> v a
  col :: m a -> Int -> v a
  det :: (Num a) => m a -> a
  matToList :: m a -> [[a]]
  matFromList :: [[a]] -> m a

data Mat2 a = Mat2 (Vec2 a) (Vec2 a) deriving(Eq)

instance (Show a) => Show (Mat2 a) where
  show (Mat2 (Vec2 a c) (Vec2 b d)) = "[ " ++ (show a) ++ " " ++ (show b) ++ " ]\n"
                                   ++ "[ " ++ (show c) ++ " " ++ (show d) ++ " ]\n"

row2 :: Mat2 a -> Int -> Vec2 a
row2 (Mat2 a b) 0 = Vec2 (a!0) (b!0)
row2 (Mat2 a b) 1 = Vec2 (a!1) (b!1)
row2 _ _ = undefined

col2 :: Mat2 a -> Int -> Vec2 a
col2 (Mat2 a _) 0 = a
col2 (Mat2 _ b) 1 = b
col2 _ _ = undefined

det2 :: (Num a) => Mat2 a -> a
det2 (Mat2 (Vec2 a c) (Vec2 b d)) = a*d - b*c

fromList2 :: [[a]] -> Mat2 a
fromList2 [[a,c],[b,d]] = Mat2 (Vec2 a c) (Vec2 b d)
fromList2 _ = undefined


instance Matrix Mat2 Vec2 where
  row = row2
  col = col2
  det = det2
  matFromList = fromList2
  matToList (Mat2 (Vec2 a c) (Vec2 b d)) = [[a,c],[b,d]]

instance (Num a) => Num (Mat2 a) where
  (Mat2 va vb) + (Mat2 ua ub) = Mat2 (va + ua) (vb + ub)
  (Mat2 va vb) - (Mat2 ua ub) = Mat2 (va - ua) (vb - ub)
  a * b = matFromList [[(row a x) <.> (col b y) | x <- [0,1]] | y <- [0,1]]
  abs m = fmap abs m
  signum m = fmap signum m
  fromInteger i = Mat2 (fromInteger i) (fromInteger i)

instance Functor Mat2 where
  fmap f m = Mat2 (fmap f $ col m 0) (fmap f $ col m 1)



data Mat3 a = Mat3 (Vec3 a) (Vec3 a) (Vec3 a) deriving(Eq)

row3 :: Mat3 a -> Int -> Vec3 a
row3 (Mat3 a b c) 0 = Vec3 (a!0) (b!0) (c!0)
row3 (Mat3 a b c) 1 = Vec3 (a!1) (b!1) (c!1)
row3 (Mat3 a b c) 2 = Vec3 (a!2) (b!2) (c!2)
row3 _ _ = undefined

col3 :: Mat3 a -> Int -> Vec3 a
col3 (Mat3 a _ _) 0 = a
col3 (Mat3 _ b _) 1 = b
col3 (Mat3 _ _ c) 2 = c
col3 _ _ = undefined

--
-- |a b c|
-- |d e f| = a |e f| - b |d f| + c |d e|
-- |g h i|     |h i|     |g i|     |g h|
--
det3 :: (Num a) => Mat3 a -> a
det3 (Mat3 (Vec3 a d g) (Vec3 b e h) (Vec3 c f i)) = a*(e*i - h*f)
                                                   - b*(d*i - g*f)
                                                   + c*(d*h - g*e)



fromList3 :: [[a]] -> Mat3 a
fromList3 [[a,b,c],[d,e,f],[g,h,i]] = Mat3 (Vec3 a d g) (Vec3 b e h) (Vec3 c f i)
fromList3 _ = undefined

instance Matrix Mat3 Vec3 where
  row = row3
  col = col3
  det = det3
  matFromList = fromList3
  matToList (Mat3 (Vec3 a d g) (Vec3 b e h) (Vec3 c f i)) = [[a,d,g],[b,e,h],[c,f,i]]

instance (Num a) => Num (Mat3 a) where
  (Mat3 va vb vc) + (Mat3 ua ub uc) = Mat3 (va + ua) (vb + ub) (vc + uc)
  (Mat3 va vb vc) - (Mat3 ua ub uc) = Mat3 (va - ua) (vb - ub) (vc - uc)
  a * b = matFromList [[(row a x) <.> (col b y) | x <- [0,1,2]] | y <- [0,1,2]]
  abs m = fmap abs m
  signum m = fmap signum m
  fromInteger i = Mat3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Functor Mat3 where
  fmap f m = Mat3 (fmap f $ col m 0) (fmap f $ col m 1) (fmap f $ col m 2)

instance (Show a) => Show (Mat3 a) where
  show (Mat3 (Vec3 a d g) (Vec3 b e h) (Vec3 c f i)) = row1 ++ row2 ++ row3
   where
    row1 = "[ "++(show a)++" "++(show b)++" "++(show c)++" ]\n"
    row2 = "[ "++(show d)++" "++(show e)++" "++(show f)++" ]\n"
    row3 = "[ "++(show g)++" "++(show h)++" "++(show i)++" ]\n"

rotx :: (Num a) => Mat3 a
rotx = matFromList [[1,0,0],[0,0,1],[0,-1,0]]

roty :: (Num a) => Mat3 a
roty = matFromList [[0,0,-1],[0,1,0],[1,0,0]]

rotz :: (Num a) => Mat3 a
rotz = matFromList [[0,1,0],[-1,0,0],[0,0,1]]




newtype Mat = [[a]]
newtype Vec = [a]

instance Matrix Mat Vec where
  row = (!!)
  col m n = map (!!n) m
  det = determinant
  matFromList = groupList
  matToList = concat

groupList l = group l m
 where
  sn = isqrt $ length l
  m = head $ filter (\a -> n `mod` a == 0) [sn..1]
  group [] n = []
  group xs n = take n xs : group (drop n xs)
  

isqrt n = converge 1 (isqrt' n') n'
 where
  converge e f x
   | (f x) - x < e = x
   | otherwise = converge e f (f x)
  isqrt' n k = isqrt ((k + n/k) / 2)
  n' = fromIntegral n

instance (Num a) => Num (Mat a) where
  (+) = zipWith (zipWith (+)) a b
  (-) = zipWith (zipWith (-)) a b
  a * b = [[sum $ zipWith (*) ra rb | rb <- (transpose b)] | ra <- a]
  abs m = fmap abs m
  signum m = fmap signum m
  fromInteger i = [[i]]

instance Functor Mat where
  fmap f = map (map f)

instance (Show a) => Show (Mat a) where
  show [] = ""
  show [(xs:xss)] = show xs ++"\n"++ show xs

determinant :: (Num a) => [[a]] -> [a]
determinant [[x]] = x
determinant xss = foldr1 (-) (zipWith (*) col1 (map determinant (minors cols)))
 where col1 = map head xss
       cols = map tail xss

minors :: [a] -> [[a]]
minors [] = []
minors (x:xs) = xs : map (x:) (minors xs)


