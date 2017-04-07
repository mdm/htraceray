module Util where

import Vector

data Camera = Camera {width :: Int, height :: Int, f :: Double}
data Intersection = Intersection Double Vector Vector deriving Show
