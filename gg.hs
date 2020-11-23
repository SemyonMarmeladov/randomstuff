-- |
dupli [1,2,3]


dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:x:dupli xs)

