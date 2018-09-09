import Ray
import Object
import Vector
import Util
import Image
import Light

-- camera = Camera 2 2 (sqrt 8)
camera = Camera 200 100 (sqrt 8)
samples = 100

world = SimpleScene [Sphere (Vector [0, 0, -1]) 0.5, Sphere (Vector [0, -100.5, -1]) 100]

intersections = do
                   r <- rays camera samples
                  --  putStrLn $ show r
                   return $ map (intersect world) r

colors = do
            i <- intersections
            return $ average samples $ map (shade $ Point (Vector [-100, 100, 100]) (Vector [1, 1, 1])) i

main = do 
          c <- colors
          save "output.png" camera c

-- main = do
--          save "output.png" camera colors
--          putStrLn $ show intersections
