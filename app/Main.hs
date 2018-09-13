import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

camera = Camera 2 3 (sqrt 8)
-- camera = Camera 400 300 (sqrt 8)
samples = 5

world = SimpleScene [Sphere (Vector [0, 0, -1]) 0.5 Diffuse, Sphere (Vector [0, -100.5, -1]) 100 Diffuse]

main = do 
          randoms1 <- return $ repeat 0.0
          putStrLn $ show $ cameraRays camera samples randoms1
        --   randoms2 <- return $ repeat 0.0
        --   save "output.png" camera $ shadeChunked samples world (cameraRays camera samples randoms1) randoms2 -- try to use only one random list

-- main = do
--          save "output.png" camera colors
--          putStrLn $ show intersections
