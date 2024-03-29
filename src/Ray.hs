module Ray where

import System.Random.Mersenne.Pure64 (PureMT, randomDouble)

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

cameraRays :: Camera -> Int -> PureMT -> [Ray]
cameraRays camera@(Camera w h _ _ _ _) samples randoms = cameraRays' w h samples camera samples randoms

cameraRays' :: Int -> Int -> Int -> Camera -> Int -> PureMT -> [Ray]
cameraRays' _ 0 _ _ _ _ = []
cameraRays' 0 y _ camera@(Camera w _ _ _ _ _) samples randoms = cameraRays' w (y - 1) samples camera samples randoms
cameraRays' x y 0 camera samples randoms = cameraRays' (x - 1) y samples camera samples randoms
cameraRays' x y s camera@(Camera w h _ _ _ _) samples randoms = cameraRay:(cameraRays' x y (s - 1) camera samples randoms'')
    where (randoms', randoms'') = split randoms
          cameraRay = cameraRay' (w - x) (y - 1) camera randoms'

cameraRay' :: Int -> Int -> Camera -> PureMT -> Ray
cameraRay' x y (Camera w h f o l u) randoms = Ray o direction'
    where (jitterX, randoms') = randomDouble randoms
          (jitterY, _) = randomDouble randoms'
          theta = fromIntegral f * pi / 180
          aspect = (fromIntegral w) / fromIntegral h
          halfHeight = tan (theta / 2)
          halfWidth = aspect * halfHeight
          _w = normalize $ Vector.subtract o l
          _u = normalize $ crossproduct u _w
          _v = crossproduct _w _u
          s = ((fromIntegral x) + jitterX) / fromIntegral w
          t = ((fromIntegral y) + jitterY) / fromIntegral h
          ms = multiplyscalar
          offset = foldl Vector.subtract o [ms halfWidth _u, ms halfHeight _v, _w]
          direction' = Vector.subtract (foldl Vector.add offset [ms (s * 2 * halfWidth) _u, ms (t * 2 * halfHeight) _v]) o

-- recurse in cameraRays, passing randoms'