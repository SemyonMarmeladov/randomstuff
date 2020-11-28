import Data.List

type Matrix = [[Double]]

x1 :: Matrix
x1 = [[1.0,2.0,3.0], [1.0,2.0,3.0]]

x2 :: Matrix
x2 = [[1.0,2.0,3.0], [1.0,2.0,3.0]]

x3 :: Matrix
x3 = [[1.0,2.0,3.0], [1.0,2.0,3.0], [1.0,2.0,3.0]]

addM, multM, multM' :: Matrix -> Matrix -> Matrix 

multM x y = multM' x (transpose y)
multM' [] [] = []
multM' (x:xs) (y:ys) = zipWith (*) x y : (multM' xs ys)


addM [] [] = []
addM (x : xs) (y : ys) = zipWith (+) x y : (addM xs ys)


