module Util where

import Vector
import Material

data Camera = Camera {width :: Int, height :: Int, fov :: Int, origin :: Vector, lookat :: Vector, up :: Vector}
data Intersection = Intersection Double Material deriving Show

instance Eq Intersection where
    (==) (Intersection t1 _) (Intersection t2 _) = t1 == t2

instance Ord Intersection where
    (<=) (Intersection t1 _) (Intersection t2 _) = t1 <= t2

average :: [Vector] -> Vector
average [xs] = xs
-- average xs = multiplyscalar (1 / (fromIntegral $ length xs)) (foldr1 Vector.add (filter (\x -> dotproduct x x > 0) xs))
average xs = multiplyscalar (1 / (fromIntegral $ length xs)) (foldr1 Vector.add xs)

randomInUnitSphere :: [Double] -> (Vector, [Double])
randomInUnitSphere randoms = head $ filter insideSphere $ map candidate (randomVectors randoms)
    where insideSphere (v, _) = (dotproduct v v) < 1
          candidate (Vector (x:y:z:[]), randoms') = (Vector.subtract (multiplyscalar 2 (Vector [x, y, z])) (Vector [1, 1, 1]), randoms')

randomVectors :: [Double] -> [(Vector, [Double])]
randomVectors randoms = (Vector xs, randoms'):(randomVectors randoms')
    where (xs, randoms') = splitAt 3 randoms

clamp :: Double -> Double
clamp x = max (min x 1) 0

-- foldlM :: (Foldable t, Monad m) => ((b -> (a -> (m b))) -> (b -> ((t a) -> (m b))))
-- foldlM f z0 xs = (((foldr f') return) xs) z0
-- where f' x k z = (((f z) x) >>= k)
