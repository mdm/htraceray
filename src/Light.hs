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
      where (shaded, randoms') = shadeSingle object ray randoms
shadeAll object (ray:rays) randoms = (shaded:rest, randoms'')
      where (shaded, randoms') = shadeSingle object ray randoms
            (rest, randoms'') = shadeAll object rays randoms'

shadeSingle :: Object -> Ray -> [Double] -> (Vector, [Double])
shadeSingle object ray randoms = (shaded, randoms')
      where (shaded, randoms') = shade object (fromIntersection $ intersect object ray) randoms

shade :: Object -> Maybe Material -> [Double] -> (Vector, [Double])
shade _ Nothing randoms = (Vector [1, 1, 1], randoms)
shade object (Just (Diffuse a i _ n)) randoms = (multiplyvector a shaded, randoms'')
    where target v = Vector.add (Vector.add i n) v
          (v, randoms') = randomInUnitSphere randoms
          reflected = Ray i (Vector.subtract (target v) i)
          (shaded, randoms'') = shadeSingle object reflected randoms'
shade object (Just (Metal a i d n)) randoms | (dotproduct (Ray.direction reflected) n) > 0 = (multiplyvector a shaded, randoms')
                                            | otherwise = (Vector [0, 0, 0], randoms)
      where reflected = Ray i (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n))
            (shaded, randoms') = shadeSingle object reflected randoms
