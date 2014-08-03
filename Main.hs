import Vector


data Camera = Camera {width :: Int, height :: Int, f :: Double}

data Ray = Ray {origin :: Vector, direction :: Vector}

data Color = Color {red :: Double, green :: Double, blue :: Double}

data Light = Light {position :: Vector, color :: Color}

data Material = Material {color :: Color}

data Object = Sphere {position :: Vector, radius :: Double, material :: Material}
              | Plane {point :: Vector, normal :: Vector, material :: Material}


rays :: Camera -> [Ray]
rays camera = [Ray (Vector [0, 0, 0]) (subtract (Vector [0, 0, 0]) (start x y)) | y <- [1..(height camera)], x <- [1..(width camera)]]
    where start = 

trace :: [Ray] -> [Light] -> [Object] -> [Color]


intersect :: Object -> Ray -> Vector
