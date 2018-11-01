import System.Random.Mersenne
import Text.Pretty.Simple (pPrint)

import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

camera = Camera 400 200 (sqrt 8)
-- camera = Camera 1920 1080 (sqrt 8)
samples = 100

world = SimpleScene [
    Sphere (Vector [0, 0, -1]) 0.5 $ Diffuse (Vector [0.8, 0.3, 0.3]),
    -- Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5,
    Sphere (Vector [0, -100.5, -1]) 100 $ Diffuse (Vector [0.8, 0.8, 0.0]),
    Sphere (Vector [1, 0, -1]) 0.5 $ Metal (Vector [0.8, 0.6, 0.2]) 1.0,
    Sphere (Vector [-1, 0, -1]) 0.5 $ Dielectric 1.5]
    -- Sphere (Vector [-1, 0, -1]) (-0.45) $ Dielectric 1.5]

main = do 
          gen <- getStdGen -- use separate gens?
          randoms1 <- randoms gen
          randoms2 <- randoms gen
          save "output.png" camera $ shadeChunked samples world (cameraRays camera samples randoms1) randoms2


-- vs :: Int -> [Double] -> [(Double, [Double])]
-- vs 0 _ = []
-- vs n randoms = (dotproduct v v, randoms'):(vs (n - 1) randoms')
--     where (v, randoms') = randomInUnitSphere randoms

-- main = do 
--           gen <- getStdGen -- use separate gens?
--           randoms1 <- randoms gen
--           pPrint $ map fst $ vs 10 randoms1


-- ray1 = Ray  (Vector [0, 0, 0]) (Vector [0, 0, -1])
-- ray2 = Ray  (Vector [0, 0, -0.5]) (Vector [0, 0, -1])

-- refract Nothing = ray1
-- refract (Just (Dielectric ri i d n)) = refracted
--     where reflected = Ray i (Vector.subtract d (multiplyscalar (2 * (dotproduct d n)) n))
--           goingOut = dotproduct d n > 0
--           normal | goingOut = multiplyscalar (-1) n
--                  | otherwise = n
--           ratio | goingOut = ri
--                 | otherwise = 1 / ri
--           normalizedD = normalize d
--           cosine = dotproduct normalizedD normal
--           discriminant = 1 - ratio * ratio * (1 - cosine * cosine)
--           refractedDirection = Vector.subtract (multiplyscalar ratio (Vector.subtract normalizedD (multiplyscalar cosine normal))) (multiplyscalar (sqrt discriminant) normal)
--           refracted = Ray (Vector.add i (multiplyscalar 0.001 refractedDirection)) refractedDirection

-- main = pPrint $ refract (fromIntersection $ intersect (Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5) ray2)
-- main = pPrint $ intersect (Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5) ray2
