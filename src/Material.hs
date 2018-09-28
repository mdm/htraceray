module Material where

import Vector

data Material = Diffuse { albedo :: Vector, point :: Vector, direction :: Vector, normal :: Vector } |
                Metal { albedo :: Vector, fuzz :: Double, point :: Vector, direction :: Vector, normal :: Vector } deriving Show
