module Util where

import Vector

data Camera = Camera {width :: Int, height :: Int, f :: Double}
data Intersection = Intersection Double Vector Vector deriving Show

instance Eq Intersection where
    (==) (Intersection t1 _ _) (Intersection t2 _ _) = t1 == t2

instance Ord Intersection where
    (<=) (Intersection t1 _ _) (Intersection t2 _ _) = t1 <= t2

average :: Int -> [Vector] -> [Vector]
average _ [] = []
average samples xs = (average' (take samples xs)):(average samples (drop samples xs))
    where average' xs = multiplyscalar (1 / (fromIntegral $ length xs)) (foldr1 Vector.add xs)