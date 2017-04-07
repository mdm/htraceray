import Ray
import Object
import Vector
import Util
import Image
import Light

camera = Camera 600 600 1

intersections = map (intersect $ Sphere (Vector [0, 0, -10]) 5) (rays camera)

colors = map (shade $ Point (Vector [-100, 100, 100]) (Vector [1, 1, 1])) intersections

main = save "output.png" camera colors
