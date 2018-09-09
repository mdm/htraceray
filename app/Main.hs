import Ray
import Object
import Vector
import Util
import Image
import Light
import Material

-- camera = Camera 2 2 (sqrt 8)
-- camera = Camera 200 100 (sqrt 8)
camera = Camera 400 300 (sqrt 8)
samples = 100

world = SimpleScene [Sphere (Vector [0, 0, -1]) 0.5 Diffuse, Sphere (Vector [0, -100.5, -1]) 100 Diffuse]

main = do 
          c <- colors world (rays camera samples) samples
          save "output.png" camera c

-- main = do
--          save "output.png" camera colors
--          putStrLn $ show intersections
