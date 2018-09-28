module Object where

import Vector
import Ray
import Material
import Util

data Object = Sphere { center :: Vector, radius :: Double, material :: (Vector -> Vector -> Vector -> Material) } |
              Plane { point :: Vector, normal :: Vector } |
              SimpleScene { objects :: [Object] }
              
-- intersect (Sphere c r) (Ray o d) | s < 0 && l2 > r2 = Nothing
--                                  | m2 > r2          = Nothing
--                                  | l2 > r2          = Just $ let t = (s - q)
--                                                                  i = multiplyscalar t d
--                                                              in Intersection t i (normalize (Vector.subtract i c))
--                                  | otherwise        = Just $ let t = (s + q)
--                                                                  i = multiplyscalar t d
--                                                              in Intersection t i (normalize (Vector.subtract i c))
--                                    where l = Vector.subtract c o 
--                                          s = dotproduct l d
--                                          l2 = dotproduct l l
--                                          r2 = r * r
--                                          m2 = l2 - s * s
--                                          q = sqrt (r2 - m2)

intersect (Sphere center radius material) (Ray origin direction) | discriminant < 0 = Nothing
                                                                 | t < 0.001 = Nothing
                                                                 | otherwise = Just $ let i = multiplyscalar t direction
                                                                                          n = normalize (Vector.subtract i center)
                                                                                      in Intersection t (material i direction n)
                                                                 where oc = Vector.subtract origin center
                                                                       a = dotproduct direction direction
                                                                       b = 2 * (dotproduct oc direction)
                                                                       c = (dotproduct oc oc) - (radius * radius)
                                                                       discriminant = (b * b) - (4 * a * c)
                                                                       t = (-b - (sqrt discriminant)) / (2 * a)

intersect (Plane p n) (Ray o d) = Nothing

intersect (SimpleScene objects) ray = closest (map ((flip intersect) ray) objects)
                                      where closest [x] = x
                                            closest (x:xs) = min' x (closest xs)
                                            min' Nothing Nothing = Nothing
                                            min' a Nothing = a
                                            min' Nothing b = b
                                            min' (Just a) (Just b) = Just (min a b)
