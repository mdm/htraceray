module Object where

import Vector
import Ray
import Util

data Object = Sphere {center :: Vector, radius :: Double} |
              Plane {point :: Vector, normal :: Vector} |
              SimpleScene {objects :: [Object]}
              
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

intersect (Sphere center radius) (Ray origin direction) | discriminant < 0 = Nothing
                                                        | otherwise = Just $ let t = (-b - (sqrt discriminant)) / (2 * a)
                                                                                 i = multiplyscalar t direction
                                                                             in Intersection t i (normalize (Vector.subtract i center))
                                                          where oc = Vector.subtract origin center
                                                                a = dotproduct direction direction
                                                                b = 2 * (dotproduct oc direction)
                                                                c = (dotproduct oc oc) - (radius * radius)
                                                                discriminant = (b * b) - (4 * a * c)

intersect (Plane p n) (Ray o d) = undefined

intersect (SimpleScene xs) (Ray o d) = undefined