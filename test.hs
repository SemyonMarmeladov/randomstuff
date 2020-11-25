ange :: Integer -> Integer -> [Integer]
ange n lim
  | n == lim  = []
  | otherwise = n : (ange (n + 1) lim)

