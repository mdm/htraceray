module Ray where

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

rays :: Camera -> [Ray]
rays (Camera w h f) = [Ray (Vector [0, 0, 0]) $  (direction x y) | y <- [(h - 1),(h - 2)..0], x <- [0..(w - 1)]]
    where horizontal | w > h = 2 * (fromIntegral w) / (fromIntegral h)
                     | otherwise = 2
          vertical | w > h = 2
                   | otherwise = 2 * (fromIntegral h) / (fromIntegral w)
          u x = (fromIntegral x) / (fromIntegral w)
          v y = (fromIntegral y) / (fromIntegral h)
          offset | w > h = Vector [-(horizontal / vertical), -1, -1]
                 | otherwise = Vector [-1, -(vertical / horizontal), -1]
          direction x y | w > h = Vector.add offset (Vector [(u x) * horizontal, (v y) * vertical, 0])
    
