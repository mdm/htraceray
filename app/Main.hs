import Ray
import Object
import Vector
import Util
import Image
import Light

camera = Camera 800 800 (sqrt 8)

world = [Sphere (Vector [-3, 3, -15]) 1, Sphere (Vector [0, 0, -20]) 5]

objectIntersections object = map (intersect object) (rays camera)

closestIntersections [x] = x
closestIntersections (x:xs) = map min' (zip x (closestIntersections xs))
                              where min' (Nothing, Nothing) = Nothing
                                    min' (a, Nothing) = a
                                    min' (Nothing, b) = b
                                    min' (Just a, Just b) = Just (min a b)

colors = map (shade $ Point (Vector [-100, 100, 100]) (Vector [1, 1, 1])) (closestIntersections (map objectIntersections world))

main = save "output.png" camera colors