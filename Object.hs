module Object where

import Vector
import Ray

data Object = Sphere {center :: Vector, radius :: Double} |
              Plane {point :: Vector, normal :: Vector}
              
intersect (Sphere c r) (Ray o d) | s < 0 && l2 > r2 = Nothing
                                 | m2 > r2          = Nothing
                                 | l2 > r2          = Just (s - q)
                                 | otherwise        = Just (s + q)
                                   where l = Vector.subtract c o 
                                         s = dotproduct l d
                                         l2 = dotproduct l l
                                         r2 = r * r
                                         m2 = l2 - s * s
                                         q = sqrt (r2 - m2)
intersect (Plane p n) (Ray o d) = undefined
