module Light where

import Vector
import Util
import Object
import Ray
import Material

data Light = Point { origin :: Vector, color :: Vector }

fromIntersection :: Maybe Intersection -> Maybe Material
fromIntersection Nothing = Nothing
fromIntersection (Just (Intersection _ material')) = Just material'

shadeChunked :: Int -> Object -> [Ray] -> [Double] -> [Vector]
shadeChunked _ _ [] _ = []
shadeChunked samples object rays randoms = (average shaded):(shadeChunked samples object rest randoms')
      where (chunk, rest) = splitAt samples rays
            (shaded, randoms') = shadeAll object chunk randoms

shadeAll :: Object -> [Ray] -> [Double] -> ([Vector], [Double])
shadeAll object (ray:[]) randoms = ([shaded], randoms')
      where (shaded, randoms') = shadeSingle 50 object ray randoms
shadeAll object (ray:rays) randoms = (shaded:rest, randoms'')
      where (shaded, randoms') = shadeSingle 50 object ray randoms
            (rest, randoms'') = shadeAll object rays randoms'

shadeSingle :: Int-> Object -> Ray -> [Double] -> (Vector, [Double])
shadeSingle 0 _ _ randoms = (Vector [0, 0, 0], randoms)
shadeSingle limit object ray randoms = (shaded, randoms')
      where (shaded, randoms') = shade limit object (fromIntersection $ intersect object ray) randoms

shade :: Int -> Object -> Maybe Material -> [Double] -> (Vector, [Double])
shade _ _ Nothing randoms = (Vector [1, 1, 1], randoms)
shade limit object (Just (Diffuse a i _ n)) randoms = (multiplyvector a shaded, randoms'')
    where target = Vector.add (Vector.add i n)
          (v, randoms') = randomInUnitSphere randoms
          reflected = Ray i (Vector.subtract (target v) i)
          (shaded, randoms'') = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Metal a f i d n)) randoms | (dotproduct (Ray.direction reflected) n) > 0 = (multiplyvector a shaded, randoms'')
                                                    | otherwise = (Vector [0, 0, 0], randoms'')
      where (v, randoms') = randomInUnitSphere randoms
            reflected = Ray i (Vector.add (multiplyscalar (clamp f) v) (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n)))
            (shaded, randoms'') = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Dielectric ri i d n)) randoms = (multiplyvector (Vector [1, 1, 1]) shaded, randoms'')
      where goingOut = dotproduct d n > 0
            normal | goingOut = multiplyscalar (-1) n
                   | otherwise = n
            ratio | goingOut = ri
                  | otherwise = 1 / ri
            normalizedD = normalize d
            cosine | goingOut = ri * (dotproduct d n) / (magnitude d)
                   | otherwise =  -1 * (dotproduct d n) / (magnitude d)
            r0 = (1 - ri) / (1 + ri)
            r0squared = r0 * r0
            schlick = r0squared + (1 - r0squared) * ((1 - cosine) ** 5)
            dt = dotproduct normalizedD normal
            discriminant = 1 - ratio * ratio * (1 - dt * dt)
            refractedDirection = Vector.subtract (multiplyscalar ratio (Vector.subtract normalizedD (multiplyscalar dt normal))) (multiplyscalar (sqrt discriminant) normal)
            refracted = Ray i refractedDirection
            reflected = Ray i (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n))
            (reflects, randoms') = (discriminant <= 0 || (head randoms) < schlick, tail randoms)
            (shaded, randoms'') | reflects = shadeSingle (limit - 1) object reflected randoms'
                                | otherwise = shadeSingle (limit - 1) object refracted randoms'
