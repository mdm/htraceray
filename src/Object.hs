module Object where

import Data.List (sortBy)

import Vector
import Ray
import Material
import Util
import AABB

data Object = Sphere { center :: Vector, radius :: Double, material :: (Vector -> Vector -> Vector -> Material) } |
              Triangle { vertices :: [Vector], material :: (Vector -> Vector -> Vector -> Material) } |
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
intersect (Triangle (p0:p1:p2:[]) material) (Ray origin direction) | a > -0.001 && a < 0.001 = Nothing
                                                          | u < 0 = Nothing
                                                          | v < 0 || u + v > 1 = Nothing
                                                          | t < 0.001 = Nothing
                                                          | otherwise = Just $ let i = Vector.add origin (multiplyscalar t direction)
                                                                                   n = normalize (crossproduct e1 e2)
                                                                               in Intersection t (material i direction n)
    where e1 = Vector.subtract p1 p0
          e2 = Vector.subtract p2 p0
          q = crossproduct direction e2
          a = dotproduct e1 q
          f = 1 / a
          s = Vector.subtract origin p0
          u = f * (dotproduct s q)
          r = crossproduct s e1
          v = f * (dotproduct direction r)
          t = f * (dotproduct e2 r)
intersect (Plane p n) (Ray o d) = Nothing
intersect (Scene objects) ray = closest (map ((flip intersect) ray) objects)
                                      where closest [] = Nothing
                                            closest [x] = x
                                            closest (x:xs) = min' x (closest xs)
intersect (BVH left right aabb) ray | hit aabb ray = min' (intersect left ray) (intersect right ray)
                                    | otherwise = Nothing

min' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
min' Nothing Nothing = Nothing
min' a Nothing = a
min' Nothing b = b
min' (Just a) (Just b) = Just (Prelude.min a b)

makeAABB :: Object -> AABB
makeAABB (Sphere center radius _) = AABB (Vector.subtract center vr) (Vector.add center vr)
    where vr = Vector [radius, radius, radius]
makeAABB (Triangle vertices _) = AABB (Vector $ foldl1 (zipWith Prelude.min) vs) (Vector $ foldl1 (zipWith Prelude.max) vs)
    where vs = map elements vertices
makeAABB (Plane _ _) = error "Cannot make AABB for unbounded object"
makeAABB (Scene []) = error "Cannot make AABB for unbounded object"
makeAABB (Scene objects) = foldl1 surround aabbs
    where aabbs = map makeAABB objects
makeAABB (BVH _ _ aabb) = aabb

makeBVH :: [Object] -> [Double] -> (Object, [Double])
makeBVH [] _ = error "Cannot build empty BVH"
makeBVH [object] randoms = (object, randoms)
makeBVH objects randoms = (BVH left' right' aabb', randoms''')
    where (axis, randoms') = (floor $ head randoms, tail randoms)
          compareObjects a b = AABB.compare (makeAABB a) (makeAABB b) axis
          (leftObjects, rightObjects) = splitAt (length objects `div` 2) (sortBy compareObjects objects)
          (left', randoms'') = makeBVH leftObjects randoms'
          (right', randoms''') = makeBVH rightObjects randoms''
          aabb' = surround (makeAABB left') (makeAABB right')
