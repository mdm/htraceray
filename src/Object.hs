module Object where

import Vector
import Ray
import Material
import Util
import AABB

data Object = Sphere { center :: Vector, radius :: Double, material :: (Vector -> Vector -> Vector -> Material) } |
              Plane { point :: Vector, normal :: Vector } |
              Scene { objects :: [Object] } |
              BVH { left :: Object, right :: Object, aabb :: AABB }

intersect :: Object -> Ray -> Maybe Intersection
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
                                                                 | otherwise = Just $ let i = Vector.add origin (multiplyscalar t direction)
                                                                                          n = normalize (Vector.subtract i center)
                                                                                      in Intersection t (material i direction n)
                                                                 where oc = Vector.subtract origin center
                                                                       a = dotproduct direction direction
                                                                       b = 2 * (dotproduct oc direction)
                                                                       c = (dotproduct oc oc) - (radius * radius)
                                                                       discriminant = (b * b) - (4 * a * c)
                                                                       candidate1 = (-b - (sqrt discriminant)) / (2 * a)
                                                                       candidate2 = (-b + (sqrt discriminant)) / (2 * a)
                                                                       t | candidate1 < 0.001 = candidate2
                                                                         | otherwise = candidate1
intersect (Plane p n) (Ray o d) = Nothing
intersect (Scene objects) ray = closest (map ((flip intersect) ray) objects)
                                      where closest [] = Nothing
                                            closest [x] = x
                                            closest (x:xs) = min' x (closest xs)
                                            min' Nothing Nothing = Nothing
                                            min' a Nothing = a
                                            min' Nothing b = b
                                            min' (Just a) (Just b) = Just (Prelude.min a b)

makeAABB :: Double -> Double -> Object -> Maybe AABB
makeAABB _ _ (Sphere center radius _) = Just $ AABB (Vector.subtract center vr) (Vector.add center vr)
    where vr = Vector [radius, radius, radius]
makeAABB _ _ (Plane _ _) = Nothing
makeAABB _ _ (Scene []) = Nothing
makeAABB t0 t1 (Scene [object]) = makeAABB t0 t1 object
makeAABB t0 t1 (Scene objects) = foldl1 surround aabbs
    where aabbs = map (makeAABB t0 t1) objects
