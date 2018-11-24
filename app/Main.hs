import System.Random.Mersenne
import Text.Pretty.Simple (pPrint)

import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

camera = Camera 400 200 45 (Vector [-2, 2, 1]) (Vector [0, 0, -1]) (Vector [0, 1, 0])
-- camera = Camera 1920 1080 45 (Vector [-2, 2, 1]) (Vector [0, 0, -1]) (Vector [0, 1, 0])
samples = 100

world = [Sphere (Vector [0, 0, -1]) 0.5 $ Diffuse (Vector [0.8, 0.3, 0.3]),
    -- Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5,
    Sphere (Vector [0, -100.5, -1]) 100 $ Diffuse (Vector [0.8, 0.8, 0.8]),
    Sphere (Vector [1, 0, -1]) 0.5 $ Metal (Vector [0.6, 0.6, 0.8]) 0.2,
    Sphere (Vector [-1, 0, -1]) 0.5 $ Dielectric 1.5]
    -- Sphere (Vector [-1, 0, -1]) (-0.45) $ Dielectric 1.5]

main = do 
          gen <- getStdGen -- use separate gens?
          randoms1 <- randoms gen
          randoms2 <- randoms gen
          randoms3 <- randoms gen
          save "output.png" camera $ shadeChunked samples (fst $ makeBVH world randoms1) (cameraRays camera samples randoms2) randoms3
