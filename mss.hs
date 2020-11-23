

nth :: Int -> Int -> [a] -> [a]
nth _ _ [] = []
nth 1 acc (_:xs) = (nth acc acc xs)
nth n acc (x:xs) = x : (nth (n-1) acc xs)

