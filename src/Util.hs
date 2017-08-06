module Util where

import Vector

data Camera = Camera {width :: Int, height :: Int, f :: Double}
data Intersection = Intersection Double Vector Vector deriving Show

instance Eq Intersection where
    (==) (Intersection t1 _ _) (Intersection t2 _ _) = t1 == t2

instance Ord Intersection where
    (<=) (Intersection t1 _ _) (Intersection t2 _ _) = t1 <= t2
