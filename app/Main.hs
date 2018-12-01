import System.Random.Mersenne
import Text.Pretty.Simple (pPrint)
import Text.Printf(printf)

import Ray
import Object
import Vector
import Util
import FileIO
import Light
import Material

-- camera = Camera 400 200 20 (Vector [13, 2, 3]) (Vector [0, 0, 0]) (Vector [0, 1, 0]) -- random world
camera = Camera 800 400 20 (Vector [0, 1, 5]) (Vector [0, 1, -1]) (Vector [0, 1, 0]) -- bunny
camera = Camera 800 400 20 (Vector [0, 1, 5]) (Vector [0, 1, -1]) (Vector [0, 1, 0]) -- suzanne
samples = 10

triangle = Triangle [Vector [0, 0, -1],Vector [0.5, 0, -1],Vector [0.5, 0.5, -1]] $ Diffuse (Vector [0.8, 0.3, 0.3])

world = [Triangle [Vector [0, 0, -1],Vector [0.5, 0, -1],Vector [0.5, 0.5, -1]] $ Diffuse (Vector [0.8, 0.3, 0.3]),
    -- Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5,
    Sphere (Vector [0, -100.5, -1]) 100 $ Diffuse (Vector [0.8, 0.8, 0.8]),
    Sphere (Vector [1, 0, -1]) 0.5 $ Metal (Vector [0.6, 0.6, 0.8]) 0.2,
    Sphere (Vector [-1, 0, -1]) 0.5 $ Dielectric 1.5]
    -- Sphere (Vector [-1, 0, -1]) (-0.45) $ Dielectric 1.5]

threeBigSpheres = [Sphere (Vector [0, 1, 0]) 1 $ Dielectric 1.5,
    Sphere (Vector [-4, 1, 0]) 1 $ Diffuse (Vector [0.4, 0.2, 0.1]),
    Sphere (Vector [4, 1, 0]) 1 $ Metal (Vector [0.7, 0.6, 0.5]) 0]

randomWorld :: [Double] -> [Object]
randomWorld randoms = ground:(randomWorld' 500 randoms)
    where ground = Sphere (Vector [0, -1000, 0]) 1000 $ Diffuse (Vector [0.5, 0.5, 0.5])

randomWorld' :: Int -> [Double] -> [Object]
randomWorld' 0 randoms = threeBigSpheres
randomWorld' n randoms = randomSphere':randomWorld' (n - 1) randoms'
    where (randomSphere', randoms') = randomSphere n randoms

randomSphere :: Int -> [Double] -> (Object, [Double])
randomSphere n randoms = (Sphere center 0.2 material, randoms'')
    where (x:z:randoms') = randoms
          center = Vector [x * 0.9 + fromIntegral (n `div` 22 - 11), 0.2, z * 0.9 + fromIntegral (n `mod` 22 - 11)]
          (material, randoms'') | head randoms' < 0.8 = randomDiffuse (tail randoms')
                                | head randoms' < 0.95 = randomMetal (tail randoms')
                                | otherwise = (Dielectric 1.5, randoms')

randomDiffuse :: [Double] -> ((Vector -> Vector -> Vector -> Material), [Double])
randomDiffuse randoms = (Diffuse (Vector [x1 * x2, y1 * y2, z1 * z2]), randoms')
    where (x1:x2:y1:y2:z1:z2:randoms') = randoms

randomMetal :: [Double] -> ((Vector -> Vector -> Vector -> Material), [Double])
randomMetal randoms = (Metal (multiplyscalar 0.5 (Vector [1 + x, 1 + y, 1 + z])) (0.5 * f), randoms')
    where (x:y:z:f:randoms') = randoms


main = do 
          gen <- getStdGen -- use separate gens?
          randoms1 <- randoms gen
          randoms2 <- randoms gen
          randoms3 <- randoms gen
        --   randoms4 <- randoms gen
          bunny <- readObjFile "suzanne.obj" $ Diffuse (Vector [0.8, 0.3, 0.3])
        --   save "output.png" camera $ shadeChunked samples (Scene bunny) (cameraRays camera samples randoms2) randoms3
          save "output.png" camera $ shadeChunked samples (fst $ makeBVH bunny randoms1) (cameraRays camera samples randoms2) randoms3
        --   save "output.png" camera $ shadeChunked samples (Scene (randomWorld randoms1)) (cameraRays camera samples randoms3) randoms4
        --   save "output.png" camera $ shadeChunked samples (fst $ makeBVH (randomWorld randoms1) randoms2) (cameraRays camera samples randoms3) randoms4

-- main = do
--           gen <- getStdGen
--           randoms1 <- randoms gen
--           pPrint $ map (intersect triangle) (cameraRays camera 1 randoms1)

-- main = do 
--           gen <- getStdGen -- use separate gens?
--         --   randoms1 <- randoms gen
--           bunny <- readObjFile "bunny.1.obj" $ Diffuse (Vector [0.8, 0.3, 0.3])
--           mapM printVertex (map elements bunny)
--     where printVertex vertex = putStrLn $ foldl1 (++) $ map (printf " %.7e") vertex