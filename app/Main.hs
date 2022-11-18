import Options.Applicative
import System.Random.Mersenne.Pure64
import Text.Pretty.Simple (pPrint)
import Text.Printf(printf)

import Config
import Ray
import Object
import Vector
import Util
import FileIO
import Light
import Material
import Transform

-- camera = Camera (width config) (height config) 20 (Vector [13, 2, 3]) (Vector [0, 0, 0]) (Vector [0, 1, 0]) -- random world
-- camera = Camera (width config) (height config) 20 (Vector [0, 1, 5]) (Vector [0, 1, -1]) (Vector [0, 1, 0]) -- bunny
camera config = Camera (Config.width config) (Config.height config) 20 (Vector [0, 0, 8]) (Vector [0, 0, -1]) (Vector [0, 1, 0]) -- suzanne

triangle = Triangle [Vector [0, 0, -1],Vector [0.5, 0, -1],Vector [0.5, 0.5, -1]] [Nothing, Nothing, Nothing] $ Diffuse (Vector [0.8, 0.3, 0.3])

world = [Triangle [Vector [0, 0, -1],Vector [0.5, 0, -1],Vector [0.5, 0.5, -1]] [Nothing, Nothing, Nothing] $ Diffuse (Vector [0.8, 0.3, 0.3]),
    -- Sphere (Vector [0, 0, -1]) 0.5 $ Dielectric 1.5,
    Sphere (Vector [0, -100.5, -1]) 100 $ Diffuse (Vector [0.8, 0.8, 0.8]),
    Sphere (Vector [1, 0, -1]) 0.5 $ Metal (Vector [0.6, 0.6, 0.8]) 0.2,
    Sphere (Vector [-1, 0, -1]) 0.5 $ Dielectric 1.5]
    -- Sphere (Vector [-1, 0, -1]) (-0.45) $ Dielectric 1.5]

threeBigSpheres = [Sphere (Vector [0, 1, 0]) 1 $ Dielectric 1.5,
    Sphere (Vector [-4, 1, 0]) 1 $ Diffuse (Vector [0.4, 0.2, 0.1]),
    Sphere (Vector [4, 1, 0]) 1 $ Metal (Vector [0.7, 0.6, 0.5]) 0]

randomWorld :: PureMT -> [Object]
randomWorld randoms = ground:(randomWorld' 500 randoms)
    where ground = Sphere (Vector [0, -1000, 0]) 1000 $ Diffuse (Vector [0.5, 0.5, 0.5])

randomWorld' :: Int -> PureMT -> [Object]
randomWorld' 0 randoms = threeBigSpheres
randomWorld' n randoms = randomSphere':randomWorld' (n - 1) randoms'
    where (randomSphere', randoms') = randomSphere n randoms

randomSphere :: Int -> PureMT -> (Object, PureMT)
randomSphere n randoms = (Sphere center 0.2 material, randoms'')
    where (x, r1) = randomDouble randoms
          (z, r2) = randomDouble r1
          (m, randoms') = randomDouble r2
          center = Vector [x * 0.9 + fromIntegral (n `div` 22 - 11), 0.2, z * 0.9 + fromIntegral (n `mod` 22 - 11)]
          (material, randoms'') | m < 0.8 = randomDiffuse randoms'
                                | m < 0.95 = randomMetal randoms'
                                | otherwise = (Dielectric 1.5, randoms')

randomDiffuse :: PureMT -> ((Vector -> Vector -> Vector -> Material), PureMT)
randomDiffuse randoms = (Diffuse (Vector [x1 * x2, y1 * y2, z1 * z2]), randoms')
    where (x1, r1) = randomDouble randoms
          (x2, r2) = randomDouble r1
          (y1, r3) = randomDouble r2
          (y2, r4) = randomDouble r3
          (z1, r5) = randomDouble r4
          (z2, randoms') = randomDouble r5

randomMetal :: PureMT -> ((Vector -> Vector -> Vector -> Material), PureMT)
randomMetal randoms = (Metal (multiplyscalar 0.5 (Vector [1 + x, 1 + y, 1 + z])) (0.5 * f), randoms')
    where (x, r1) = randomDouble randoms
          (y, r2) = randomDouble r1
          (z, r3) = randomDouble r2
          (f, randoms') = randomDouble r3

g = Sphere (Vector [0, -1000, 0]) 950 $ Diffuse (Vector [0.5, 0.5, 0.5])

main = do 
          parsedOpts <- execParser options
          randoms <- newPureMT -- use separate gens?
          bunny <- readObjFile "suzanne2.obj" $ Diffuse (Vector [0.8, 0.3, 0.3])
        --   bunny <- readObjFile "suzanne2.obj" $ fst $ randomMetal randoms
        --   save "output.png" camera $ shadeChunked samples (Scene bunny) (cameraRays camera samples randoms2) randoms3
          save (outputFile parsedOpts) (camera parsedOpts) $ shadeChunked parsedOpts (TransformWrapper (Rotate (Vector [0, 1, 0]) 45) $ makeBVH (g:bunny) randoms) (cameraRays (camera parsedOpts) (samples parsedOpts) randoms) randoms
        --   save "output.png" camera $ shadeChunked samples (Scene (randomWorld randoms)) (cameraRays camera samples randoms) randoms
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
