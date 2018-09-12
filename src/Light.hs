module Light where

import Vector
import Util
import Object
import Ray
import Material

data Light = Point { origin :: Vector, color :: Vector }

fromIntersection Nothing = Nothing
fromIntersection (Just (Intersection t material)) = Just material

shadeChunked :: Int -> Object -> [Ray] -> [Double] -> [Vector]
shadeChunked _ _ [] _ = []
shadeChunked samples object rays randoms = (average shaded):(shadeChunked samples object rest randoms')
      where (chunk, rest) = splitAt samples rays
            (shaded, randoms') = shadeAll object chunk randoms

shadeAll :: Object -> [Ray] -> [Double] -> [Vector]
shadeAll object (ray:rays) randoms = shaded:(shadeAll object rays randoms')
      where (shaded, randoms') = shadeSingle object ray randoms

      (map (fromIntersection . intersect object)
shadeSingle :: Object -> Ray -> [Double] -> Vector
shadeSingle _ _ Nothing = Vector [1, 1, 1]

shade :: Object -> Maybe Material -> [Double] -> Vector
shade _ Nothing _ = Vector [1, 1, 1]
shade object (Just (Diffuse i n)) randoms = multiplyscalar 0.5 $ shadeSingle object reflected randoms'
    where target v = Vector.add (Vector.add i n) v
          (v, randoms') = randomInUnitSphere randoms
          reflected = Ray i (Vector.subtract (target v) i)
