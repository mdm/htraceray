import System.Random.Mersenne

import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

-- camera = Camera 200 100 (sqrt 8)
camera = Camera 1920 1080 (sqrt 8)
samples = 100

world = SimpleScene [Sphere (Vector [0, 0, -1]) 0.5 Diffuse, Sphere (Vector [0, -100.5, -1]) 100 Diffuse]

main = do 
          gen <- getStdGen -- use separate gens?
          randoms1 <- randoms gen
          randoms2 <- randoms gen
          save "output.png" camera $ shadeChunked samples world (cameraRays camera samples randoms1) randoms2
