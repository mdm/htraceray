{-# LANGUAGE ExistentialQuantification #-}

module Object where

import Data.List (sortBy)
import System.Random.Mersenne.Pure64 (PureMT, randomDouble)

import Vector
import Ray
import Material
import Util
import AABB
import Transform

class Object a where
    intersect :: a -> Ray -> Maybe Intersection
    makeAABB :: a -> AABB
    makeAABB _ = error "makeAABB not implemented"


data Sphere = Sphere
    Vector -- centet
    Double -- radius
    (Vector -> Vector -> Vector -> Material) -- material

instance Object Sphere where
    intersect (Sphere center radius material) (Ray origin direction) | discriminant < 0 = Nothing
                                                                     | t < epsilon = Nothing
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
              t | candidate1 < epsilon = candidate2
                | otherwise = candidate1
    makeAABB (Sphere center radius _) = AABB (Vector.subtract center vr) (Vector.add center vr)
        where vr = Vector [radius, radius, radius]


data Triangle = Triangle
    [Vector] -- vertices
    [Maybe Vector] -- normals
    (Vector -> Vector -> Vector -> Material) -- material

instance Object Triangle where
    intersect (Triangle ps@(p0:p1:p2:[]) normals material) (Ray origin direction) | a > epsilon && a < epsilon = Nothing
                                                                                  | u < 0 = Nothing
                                                                                  | v < 0 || u + v > 1 = Nothing
                                                                                  | t < epsilon = Nothing
                                                                                  | otherwise = Just $ Intersection t (material i direction n)
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
              ns = map (maybe (normalize (crossproduct e1 e2)) id) normals
              area a b c = (magnitude $ crossproduct (Vector.subtract b a) (Vector.subtract c a)) / 2
              i = Vector.add origin (multiplyscalar t direction)
              ws = [area p1 p2 i / area p0 p1 p2, area p2 p0 i / area p0 p1 p2, area p0 p1 i / area p0 p1 p2]
              n = foldr1 Vector.add $ zipWith multiplyscalar ws ns
    makeAABB (Triangle vertices _ _) = AABB (Vector $ foldl1 (zipWith Prelude.min) vs) (Vector $ foldl1 (zipWith Prelude.max) vs)
        where vs = map elements vertices

data Plane = Plane
    Vector -- point
    Vector -- normal

instance Object Plane where
    intersect (Plane p n) (Ray o d) = Nothing

data ConcreteObject = forall a. Object a => ConcreteObject a

instance Object ConcreteObject where
    intersect (ConcreteObject a) = intersect a

data Scene = Scene
    [ConcreteObject] -- objects

instance Object Scene where
    intersect (Scene objects) ray = closest (map ((flip intersect) ray) objects)
        where closest [] = Nothing
              closest [x] = x
              closest (x:xs) = min' x (closest xs)
    makeAABB (Scene objects) = foldl1 surround aabbs
        where aabbs = map makeAABB objects

data BVH = BVH
    ConcreteObject -- left
    ConcreteObject -- right
    AABB -- aabb

instance Object BVH where
    intersect (BVH left right aabb) ray | hit aabb ray = min' (intersect left ray) (intersect right ray)
                                        | otherwise = Nothing
    makeAABB (BVH _ _ aabb) = aabb

data TransformWrapper = TransformWrapper
    Transform -- transform
    ConcreteObject -- object

instance Object TransformWrapper where
    intersect (TransformWrapper transform object) (Ray origin direction) = fmap (transformIntersection transform) $ intersect object (Ray o' d')
        where o' = Transform.applyInverse transform origin
              d' = Transform.applyInverse transform direction

epsilon = 0.00001

transformIntersection transform (Intersection t material) = Intersection t material'
    where i' = Transform.apply transform False $ Material.point material
          d' = Transform.apply transform False $ Material.direction material
          n' = Transform.apply transform True $ Material.normal material
          material' = material { Material.point = i', Material.direction = d', Material.normal = n' }

min' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
min' Nothing Nothing = Nothing
min' a Nothing = a
min' Nothing b = b
min' (Just a) (Just b) = Just (Prelude.min a b)

makeBVH :: [ConcreteObject] -> PureMT -> ConcreteObject
makeBVH [] _ = error "Cannot build empty BVH"
makeBVH [object] _ = ConcreteObject object
makeBVH objects randoms = ConcreteObject $ BVH left' right' aabb'
    where axis = floor . fst $ randomDouble randoms
          (randoms', randoms'') = split randoms
          compareObjects a b = AABB.compare (makeAABB a) (makeAABB b) axis
          (leftObjects, rightObjects) = splitAt (length objects `div` 2) (sortBy compareObjects objects)
          left' = makeBVH leftObjects randoms'
          right' = makeBVH rightObjects randoms''
          aabb' = surround (makeAABB left') (makeAABB right')
