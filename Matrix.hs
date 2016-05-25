module Matrix where

import Vector

data Matrix = Matrix {rows :: [Vector]} deriving Show

multiplyvector matrix vector = Vector $ map (Vector.dotproduct vector) (rows matrix)

transpose matrix = Matrix [Vector $ map (!!x) (map elements (rows matrix)) | x <- [0..(length (elements (head (rows matrix)))) - 1]]

multiplymatrix lhs rhs = Matrix $ map (Matrix.multiplyvector lhs) (rows (transpose rhs))
