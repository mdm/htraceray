module Vector where

data Vector = Vector {elements :: [Double]} deriving Show

add (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
    
subtract (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
    
multiplyvector (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)

multiplyscalar s (Vector xs) = Vector (map (*s) xs)
    
sumvector (Vector xs) = sum xs
    
dotproduct :: Vector -> Vector -> Double
dotproduct x y = sumvector (multiplyvector x y)

magnitude :: Vector -> Double
magnitude x = sqrt (dotproduct x x)

normalize :: Vector -> Vector
normalize x@(Vector xs) = Vector (map (/ magnitude x) xs)

crossproduct (Vector xs) (Vector ys) = let a = xs!!1 * ys!!2 - xs!!2 * ys!!1
                                           b = xs!!2 * ys!!0 - xs!!0 * ys!!2
                                           c = xs!!0 * ys!!1 - xs!!1 * ys!!0
                                       in Vector [a, b, c]
                                       
