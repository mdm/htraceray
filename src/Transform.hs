module Transform where

import Vector
import Matrix

data Transform = Scale { factors :: Vector } | Translate { offset :: Vector } | Rotate { axis :: Vector, degrees :: Double }

apply :: Transform -> Vector -> Vector
apply (Scale f) v = Vector.multiplyvector f v
apply (Translate o) v = Vector.add o v
apply (Rotate a d) v = Matrix.multiplyvector m v
    where a' = normalize a
          d' = d * pi / 180
          m = rotationMatrix' a' d'

applyInverse :: Transform -> Vector -> Vector
applyInverse (Scale f) v = Vector.multiplyvector (invert f) v
    where invert (Vector fs) = Vector $ map (1/) fs
applyInverse (Translate o) v = flip Vector.subtract o v
applyInverse (Rotate a d) v = Matrix.multiplyvector m v
    where a' = normalize a
          d' = -d * pi / 180
          m = rotationMatrix' a' d'

rotationMatrix :: Vector -> Double -> Matrix -- this implementation has bugs!
rotationMatrix a@(Vector (ax:ay:az:[])) phi = foldr1 multiplymatrix [mt, rx, m]
    where s' | abs ax <= abs ay && abs ax <= abs az = Vector [0, -az, ay]
             | abs ay <= abs ax && abs ay <= abs az = Vector [-az, 0, ax]
             | otherwise = Vector [-ay, ax, 0]
          s = normalize s'
          t = crossproduct a s
          m = Matrix [a, s, t]
          mt = transpose m
          rx = Matrix [Vector [1, 0, 0], Vector [0, cos phi, -sin phi], Vector [0, sin phi, cos phi]]

rotationMatrix' :: Vector -> Double -> Matrix
rotationMatrix' a@(Vector (ax:ay:az:[])) phi = Matrix [Vector [cos phi + (1 - cos phi) * ax * ax, (1 - cos phi) * ax * ay - az * sin phi, (1 - cos phi) * ax * az + ay * sin phi],
    Vector [(1 - cos phi) * ax * ay + az * sin phi, cos phi + (1 - cos phi) * ay * ay, (1 - cos phi) * ay * az - ax * sin phi],
    Vector [(1 - cos phi) * ax * az - ay * sin phi, (1 - cos phi) * ay * az + ax * sin phi, cos phi + (1 - cos phi) * az * az]]
