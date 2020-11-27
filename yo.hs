import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- getStdGen
  return . take n $ randomRs (1, m) gen 
