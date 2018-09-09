module Light where

import Vector
import Util
import Object
import Ray
import Material

data Light = Point { origin :: Vector, color :: Vector }

fromIntersection Nothing = Nothing
fromIntersection (Just (Intersection t material)) = Just material

colors :: Object -> IO [Ray] -> Int -> IO [Vector]
colors object rays samples = do
                                 r <- rays
                                 shaded <- mapM (shade object []) (map (fromIntersection . intersect object) r)
                                 return $ average samples $ shaded

shade :: Object -> [Light] -> Maybe Material -> IO Vector
shade _ _ Nothing = return $ Vector [1, 1, 1]
shade object _ (Just (Diffuse i n)) = do
                                          color <- colors object (sequence [reflected]) 1
                                          return $ multiplyscalar 0.5 (head color)
    where target v = Vector.add (Vector.add i n) v
          reflected = do 
                         v <- randomInUnitSphere
                         return $ Ray i (Vector.subtract (target v) i)
