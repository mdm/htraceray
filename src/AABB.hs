module AABB where
    
import Vector
import Ray

data AABB = AABB {min :: Vector, max :: Vector}

hit :: AABB -> Ray -> Double -> Double -> Bool
hit (AABB _min _max) (Ray origin (Vector direction)) tmin tmax = all (==False) disjoint
        where candidates1 = elements $ multiplyvector (Vector.subtract _min origin) (Vector $ map (1/) direction)
              candidates2 = elements $ multiplyvector (Vector.subtract _max origin) (Vector $ map (1/) direction)
              t0 = zipWith Prelude.min candidates1 candidates2
              t1 = zipWith Prelude.max candidates1 candidates2
              tmins = tail $ scanl Prelude.max tmin t0
              tmaxs = tail $ scanl Prelude.min tmax t1
              disjoint = zipWith (<=) tmaxs tmins

surround :: Maybe AABB -> Maybe AABB -> Maybe AABB
surround Nothing _ = Nothing
surround _ Nothing = Nothing
surround (Just (AABB minA maxA)) (Just (AABB minB maxB)) = Just $ AABB small big
        where small = Vector $ zipWith Prelude.min (elements minA) (elements minB)
              big = Vector $ zipWith Prelude.max (elements maxA) (elements maxB)
