module AABB where
    
import Vector
import Ray

data AABB = AABB {min :: Vector, max :: Vector}

hit :: AABB -> Ray -> Bool
hit (AABB _min _max) (Ray origin (Vector direction)) = all (==False) disjoint
        where candidates1 = elements $ multiplyvector (Vector.subtract _min origin) (Vector $ map (1/) direction)
              candidates2 = elements $ multiplyvector (Vector.subtract _max origin) (Vector $ map (1/) direction)
              t0 = zipWith Prelude.min candidates1 candidates2
              t1 = zipWith Prelude.max candidates1 candidates2
              tmin = scanl1 Prelude.max t0
              tmax = scanl1 Prelude.min t1
              disjoint = zipWith (<=) tmax tmin

compare :: AABB -> AABB -> Int -> Ordering
compare a b axis | (elements $ AABB.min a)!!axis < (elements $ AABB.min b)!!axis = LT
                               | otherwise = GT

surround :: AABB -> AABB -> AABB
surround (AABB minA maxA) (AABB minB maxB) = AABB small big
        where small = Vector $ zipWith Prelude.min (elements minA) (elements minB)
              big = Vector $ zipWith Prelude.max (elements maxA) (elements maxB)
