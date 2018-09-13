module Ray where

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

cameraRays :: Camera -> Int -> [Double] -> [Ray]
cameraRays (Camera w h f) samples randoms = cameraRays' w h samples w h samples randoms

cameraRays' :: Int -> Int -> Int -> Int -> Int -> Int -> [Double] -> [Ray]
cameraRays' _ 0 _ _ _ _ _ = []
cameraRays' 0 y _ w h samples randoms = cameraRays' w (y - 1) samples w h samples randoms
cameraRays' x y 0 w h samples randoms = cameraRays' (x - 1) y samples w h samples randoms
cameraRays' x y s w h samples randoms = cameraRay:(cameraRays' x y (s - 1) w h samples randoms')
    where (cameraRay, randoms') = cameraRay' (fromIntegral (w - x)) (fromIntegral (y - 1)) (fromIntegral w) (fromIntegral h) randoms

cameraRay' :: Double -> Double -> Double -> Double -> [Double] -> (Ray, [Double])
cameraRay' x y w h (jitterX:jitterY:randoms') = (Ray (Vector [0, 0, 0]) direction', randoms')
    where horizontal | w > h = 2 * w / h
                     | otherwise = 2
          vertical | w > h = 2
                   | otherwise = 2 * h / w
          u = (x + jitterX) / w
          v = (y + jitterY) / h
          offset | w > h = Vector [-(horizontal / vertical), -1, -1]
                 | otherwise = Vector [-1, -(vertical / horizontal), -1]
          direction' = Vector.add offset (Vector [u * horizontal, v * vertical, 0])

-- recurse in cameraRays, passing randoms'