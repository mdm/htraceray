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
    where target v = Vector.add (Vector.add i n) v
          (v, randoms') = randomInUnitSphere randoms
          reflected = Ray i (Vector.subtract (target v) i)
          (shaded, randoms'') = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Metal a f i d n)) randoms | (dotproduct (Ray.direction reflected) n) > 0 = (multiplyvector a shaded, randoms'')
                                                    | otherwise = (Vector [0, 0, 0], randoms'')
      where (v, randoms') = randomInUnitSphere randoms
            reflected = Ray i (Vector.add (multiplyscalar (clamp f) v) (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n)))
            (shaded, randoms'') = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Dielectric ri i d n)) randoms | discriminant > 0 = (multiplyvector (Vector [1, 1, 1]) shaded, randoms')
                                                        | otherwise = (Vector [0, 0, 0], randoms')
      where reflected = Ray i (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n))
            goingOut = dotproduct d n > 0
            normal | goingOut = multiplyscalar (-1) n
                   | otherwise = n
            ratio | goingOut = ri
                  | otherwise = 1 / ri
            normalizedD = normalize d
            cosine = dotproduct normalizedD normal
            discriminant = 1 - ratio * ratio * (1 - cosine * cosine)
            refractedDirection = Vector.subtract (multiplyscalar ratio (Vector.subtract normalizedD (multiplyscalar cosine normal))) (multiplyscalar (sqrt discriminant) normal)
            -- refracted = Ray (Vector.add i (multiplyscalar 0.001 refractedDirection)) refractedDirection
            refracted = Ray i refractedDirection
            (shaded, randoms') = shadeSingle (limit - 1) object refracted randoms