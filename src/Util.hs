module Util where

import Control.Monad.Loops
import System.Random.Mersenne

import Vector
import Material

data Camera = Camera {width :: Int, height :: Int, f :: Double}
data Intersection = Intersection Double Material deriving Show

instance Eq Intersection where
    (==) (Intersection t1 _) (Intersection t2 _) = t1 == t2

instance Ord Intersection where
    (<=) (Intersection t1 _) (Intersection t2 _) = t1 <= t2

average :: Int -> [Vector] -> [Vector]
average _ [] = []
average 1 xs = xs
average samples xs = (average' (take samples xs)):(average samples (drop samples xs))
    where average' xs = multiplyscalar (1 / (fromIntegral $ length xs)) (foldr1 Vector.add xs)

randomInUnitSphere :: IO Vector
randomInUnitSphere = iterateUntil insideSphere randomVector
    where insideSphere v = (dotproduct v v) < 1
          randomVector = do x <- (randomIO :: IO Double)
                            y <- (randomIO :: IO Double)
                            z <- (randomIO :: IO Double)
                            return $ Vector.subtract (multiplyscalar 2 (Vector [x, y, z])) (Vector [1, 1, 1])