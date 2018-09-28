import System.Random.Mersenne
-- import Text.Pretty.Simple (pPrint)

import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

camera = Camera 200 100 (sqrt 8)
-- camera = Camera 1920 1080 (sqrt 8)
samples = 50

world = SimpleScene [
    Sphere (Vector [0, 0, -1]) 0.5 $ Diffuse (Vector [0.8, 0.3, 0.3]),
    Sphere (Vector [0, -100.5, -1]) 100 $ Diffuse (Vector [0.8, 0.8, 0.0]),
    Sphere (Vector [1, 0, -1]) 0.5 $ Metal (Vector [0.8, 0.6, 0.2]) 1.0,
    Sphere (Vector [-1, 0, -1]) 0.5 $ Dielectric 1.5]

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
