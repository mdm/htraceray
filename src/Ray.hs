module Ray where

import System.Random

import Vector
import Util

data Ray = Ray {origin :: Vector, direction :: Vector} deriving Show

rays :: Camera -> Int -> IO [Ray]
rays (Camera w h f) samples = sequence $ do
                                            y <- [(h - 1),(h - 2)..0]
                                            x <- [0..(w - 1)]
                                            _ <- [1..samples]
                                            return $ do
                                                        jitterX <- (randomIO :: IO Double)
                                                        jitterY <- (randomIO :: IO Double)
                                                        return $ Ray (Vector [0, 0, 0]) (direction x y jitterX jitterY)
                                 
    where horizontal | w > h = 2 * (fromIntegral w) / (fromIntegral h)
                     | otherwise = 2
          vertical | w > h = 2
                   | otherwise = 2 * (fromIntegral h) / (fromIntegral w)
          u x jitter = ((fromIntegral x) + jitter) / (fromIntegral w)
          v y jitter = ((fromIntegral y) + jitter) / (fromIntegral h)
          offset | w > h = Vector [-(horizontal / vertical), -1, -1]
                 | otherwise = Vector [-1, -(vertical / horizontal), -1]
          direction x y jitterX jitterY = Vector.add offset (Vector [(u x jitterX) * horizontal, (v y jitterY) * vertical, 0])
