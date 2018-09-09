module Material where

import Vector

data Material = Diffuse { point :: Vector, normal :: Vector } deriving Show
