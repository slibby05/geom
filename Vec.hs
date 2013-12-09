
module Vec 
(Vector,
 Vec2(..),
 Vec3(..),
 z2, i2, j2,
 z3, i3, j3, k3,
 (<.>),
 (.>),
 (!),
 vecFromList,
 vecToList,
 norm
)where

class Vector v where
    (<.>) :: (Num a) => v a -> v a -> a
    (.>) :: (Num a) => a -> v a -> v a
    (!) :: v a -> Int -> a
    vecFromList :: [a] -> v a
    vecToList :: v a -> [a]

norm :: (Vector v) => (Num a) => v a -> a
norm v = v <.> v

data Vec2 a = Vec2 a a deriving (Eq, Ord)

fromList2 :: [a] -> Vec2 a
fromList2 (x:y:_) = Vec2 x y
fromList2 _ = undefined

ix2 :: Vec2 a -> Int -> a
ix2 (Vec2 x y) 0 = x
ix2 (Vec2 x y) 1 = y
ix2 (Vec2 x y) _ = undefined

instance Functor Vec2 where
  fmap f (Vec2 x y) = Vec2 (f x) (f y)

instance (Num a) => Num (Vec2 a) where
  (+) (Vec2 ux uy) (Vec2 vx vy) = Vec2 (ux + vx) (uy + vy)
  (-) (Vec2 ux uy) (Vec2 vx vy) = Vec2 (ux - vx) (uy - vy)
  (*) (Vec2 ux uy) (Vec2 vx vy) = Vec2 (ux * vx) (uy * vy)
  abs v = fmap abs v
  signum v = fmap signum v
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)

instance Vector Vec2 where
  Vec2 ux uy <.> Vec2 vx vy = (ux * vx) + (uy * vy)
  a .> Vec2 x y = Vec2 (a * x) (a * y)
  (!) = ix2
  vecFromList = fromList2
  vecToList (Vec2 x y) = [x,y]

instance (Show a) => Show (Vec2 a) where
  show (Vec2 a b) = "<"++(show a)++","++(show b)++">"

z2 :: Vec2 Float
z2 = Vec2 0 0

i2 :: Vec2 Float
i2 = Vec2 1 0

j2 :: Vec2 Float
j2 = Vec2 0 1



data Vec3 a = Vec3 a a a  deriving (Eq, Ord)

ix3 :: Vec3 a -> Int -> a
ix3 (Vec3 x _ _) 0 = x
ix3 (Vec3 _ y _) 1 = y
ix3 (Vec3 _ _ z) 2 = z
ix3 _ _ = undefined

fromList3 :: [a] -> Vec3 a
fromList3 (x:y:z:_) = Vec3 x y z
fromList3 _ = undefined

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Num a => Num (Vec3 a) where
  (+) (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux + vx) (uy + vy) (uz + vz)
  (-) (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux - vx) (uy - vy) (uz - vz)
  (*) (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux * vx) (uy * vy) (uz * vz)
  abs v = fmap abs v
  signum v = fmap signum v
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Vector Vec3 where
  Vec3 ux uy uz <.> Vec3 vx vy vz = (ux * vx) + (uy * vy) + (uz * vz)
  a .> Vec3 x y z = Vec3 (a * x) (a * y) (a * z)
  (!) = ix3
  vecFromList = fromList3
  vecToList (Vec3 x y z) = [x,y,z]

z3 :: Vec3 Float
z3 = Vec3 0 0 0

i3 :: Vec3 Float
i3 = Vec3 1 0 0

j3 :: Vec3 Float
j3 = Vec3 0 1 0

k3 :: Vec3 Float
k3 = Vec3 0 0 1

instance (Show a) => Show (Vec3 a) where
  show (Vec3 a b c) = "<"++(show a)++","++(show b)++","++(show c)++">"

