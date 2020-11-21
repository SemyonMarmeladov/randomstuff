

inits :: [a] -> [[a]]
inits []   = [[]]
inits (x:xs) = [] : map (x:) (inits xs)


tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs):tails xs


mss :: [Int] -> Int
mss = maximum . map sum . segments

segments = concat . map inits . tails
