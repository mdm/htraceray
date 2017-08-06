module Ray where

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

rays :: Camera -> [Ray]
rays (Camera w h f) = [Ray (Vector [0, 0, 0]) $ normalize (Vector.subtract (Vector [0, 0, 0]) (start x y)) | y <- [0..(h - 1)], x <- [0..(w - 1)]]
    where start x y = Vector [-2 * (fromIntegral x + 0.5) / (fromIntegral w - 1) + 1, 2 * (fromIntegral y + 0.5) / (fromIntegral h) - 1, f]
    
