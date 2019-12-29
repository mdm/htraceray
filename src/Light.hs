module Light where

import Control.Parallel.Strategies
import System.Random.Mersenne.Pure64 (PureMT, randomDouble)

import Vector
import Util
import Object
import Ray
import Material

data Light = Point { origin :: Vector, color :: Vector }

fromIntersection :: Maybe Intersection -> Maybe Material
fromIntersection Nothing = Nothing
fromIntersection (Just (Intersection _ material')) = Just material'

shadeChunked :: Int -> Object -> [Ray] -> PureMT -> [Vector]
shadeChunked _ _ [] _ = []
-- shadeChunked samples object rays randoms = shadedChunk:shadedRest `using` parListN 4 rpar
shadeChunked samples object rays randoms = shadedChunk:shadedRest
      where (chunk, rest) = splitAt samples rays
            shadedChunk = average $ shadeAll object chunk randoms
            shadedRest = shadeChunked samples object rest randoms

shadeAll :: Object -> [Ray] -> PureMT -> [Vector]
shadeAll object (ray:[]) randoms = [shaded]
      where shaded = shadeSingle 50 object ray randoms
shadeAll object (ray:rays) randoms = shaded:rest
      where shaded = shadeSingle 50 object ray randoms
            rest = shadeAll object rays randoms

shadeSingle :: Int-> Object -> Ray -> PureMT -> Vector
shadeSingle 0 _ _ _ = Vector [0, 0, 0]
shadeSingle limit object ray randoms = shade limit object (fromIntersection $ intersect object ray) randoms

shade :: Int -> Object -> Maybe Material -> PureMT -> Vector
shade _ _ Nothing randoms = Vector [1, 1, 1]
shade limit object (Just (Diffuse a i _ n)) randoms = multiplyvector a shaded
    where target = Vector.add (Vector.add i n)
          (v, randoms') = randomInUnitSphere randoms
          reflected = Ray i (Vector.subtract (target v) i)
          shaded = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Metal a f i d n)) randoms | (dotproduct (Ray.direction reflected) n) > 0 = multiplyvector a shaded
                                                    | otherwise = Vector [0, 0, 0]
      where (v, randoms') = randomInUnitSphere randoms
            reflected = Ray i (Vector.add (multiplyscalar (clamp f) v) (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n)))
            shaded = shadeSingle (limit - 1) object reflected randoms'
shade limit object (Just (Dielectric ri i d n)) randoms = multiplyvector (Vector [1, 1, 1]) shaded
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
            (random, randoms') = randomDouble randoms
            reflects = discriminant <= 0 || random < schlick
            shaded | reflects = shadeSingle (limit - 1) object reflected randoms'
                   | otherwise = shadeSingle (limit - 1) object refracted randoms'
