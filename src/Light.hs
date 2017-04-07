module Light where

import Vector
import Util

data Light = Point {origin :: Vector, color :: Vector}

shade _ Nothing = Vector [1, 1, 1]
shade (Point o c) (Just (Intersection t i n)) = multiplyscalar (dotproduct n (normalize $ Vector.subtract o i)) (Vector [1, 0, 0])
