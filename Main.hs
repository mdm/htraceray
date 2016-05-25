import Vector
import Matrix

data Camera = Camera {width :: Int, height :: Int, f :: Double}

data Ray = Ray {origin :: Vector, direction :: Vector}

data Color = Color {red :: Double, green :: Double, blue :: Double}

data Light = Light {lightPosition :: Vector, lightColor :: Color}

data Material = Material {color :: Color}

data Object = Sphere {position :: Vector, radius :: Double, material :: Material}
              | Plane {point :: Vector, normal :: Vector, material :: Material}


rays :: Camera -> [Ray]
rays (Camera w h f) = [Ray (Vector [0, 0, 0]) $ normalize (Vector.subtract (Vector [0, 0, 0]) (start x y)) | y <- [1..h], x <- [1..w]]
    where start x y = Vector [2 * x / w - 1, 2 * y / h - 1, f]

--trace :: [Ray] -> [Light] -> [Object] -> [Color]


--intersect :: Object -> Ray -> Maybe Double
--intersect (Sphere position radius _) (Ray origin direction) = 

--flatten

