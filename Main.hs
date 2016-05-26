import Ray
import Object
import Vector
import Util
import Image

camera = Camera 600 600 1

intersections = map (intersect $ Sphere (Vector [0, 0, -10]) 5) (rays camera)

color (Just _) = Color 1 0 0
color Nothing  = Color 1 1 1

colors = map color intersections

main = save "output.png" camera colors
