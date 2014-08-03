module Vector where

data Vector = Vector [Double] deriving Show


add (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
    
subtract (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
    
multiplyvector (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)

multiplyscalar s (Vector xs) = Vector (map (*s) xs)
    
sumvector (Vector xs) = sum xs
    
dotproduct :: Vector -> Vector -> Double
dotproduct x y = sumvector (multiplyvector x y)

vectorlength :: Vector -> Double
vectorlength a = sqrt (dotproduct a a)

normalize :: Vector -> Vector
normalize x@(Vector xs) = Vector (map (/ vectorlength x) xs)
