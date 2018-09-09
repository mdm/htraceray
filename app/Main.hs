import Ray
import Object
import Vector
import Util
import Image
import Light

-- camera = Camera 10 10 (sqrt 8)
camera = Camera 800 600 (sqrt 8)

world = SimpleScene [Sphere (Vector [0, 0, -1]) 0.5, Sphere (Vector [0, -100.5, -1]) 100]

intersections = map (intersect world) (rays camera)
colors = map (shade $ Point (Vector [-100, 100, 100]) (Vector [1, 1, 1])) intersections

main = save "output.png" camera colors
-- main = do
--          save "output.png" camera colors
--          putStrLn $ show intersections