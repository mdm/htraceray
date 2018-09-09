import Ray
import Object
import Vector
import Util
import Image
import Light

camera = Camera 800 600 (sqrt 8)

world = [Sphere (Vector [0, 0, -1]) 0.5]

objectIntersections object = map (intersect object) (rays camera)

closestIntersections [x] = x
closestIntersections (x:xs) = map min' (zip x (closestIntersections xs))
                              where min' (Nothing, Nothing) = Nothing
                                    min' (a, Nothing) = a
                                    min' (Nothing, b) = b
                                    min' (Just a, Just b) = Just (min a b)

colors = map (shade $ Point (Vector [-100, 100, 100]) (Vector [1, 1, 1])) (closestIntersections (map objectIntersections world))

dummyColors = map dummyShade $ objectIntersections $ head world
    where dummyShade Nothing = Vector [1, 1, 1]
          dummyShade (Just _) = Vector [1, 0, 0]

main = do 
         save "output.png" camera colors
      --    putStrLn $ show $ rays camera
      --    putStrLn $ show $ objectIntersections $ head world
      --    putStrLn $ show $ intersect (head world) (Ray (Vector [0, 0, 0]) (Vector [-1, 1, -1]))
      --    putStrLn $ show $ intersect (head world) (Ray (Vector [0, 0, 0]) (Vector [0, 0, -1]))