module Matrix where

import Vector

data Matrix = Matrix {rows :: [Vector]} deriving Show

multiplyvector matrix vector = Vector $ map (Vector.dotproduct vector) (rows matrix)

transpose (Matrix ((Vector []):_)) = Matrix []
transpose matrix = Matrix $ (Vector $ map (head . elements) (rows matrix)) : (rows $ transpose (Matrix $ map (Vector . tail . elements) (rows matrix)))

multiplymatrix lhs rhs = Matrix $ map (Matrix.multiplyvector lhs) (rows (transpose rhs))
