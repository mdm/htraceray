module Ray where

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

rays :: Camera -> [Ray]
rays (Camera w h f) = [Ray (Vector [0, 0, 0]) $ normalize (Vector.subtract (Vector [0, 0, 0]) (start x y)) | y <- [1..h], x <- [1..w]]
    where start x y = Vector [2 * fromIntegral x / fromIntegral w - 1, 2 * fromIntegral y / fromIntegral h - 1, f]
    
