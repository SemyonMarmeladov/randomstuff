import Control.Monad
import Data.List

l1 :: [Int]
l1 = [1,2,3]

m :: [Int] -> [Int] 
m xs = do
    (y:ys) <- tails xs
    (y:ys)
