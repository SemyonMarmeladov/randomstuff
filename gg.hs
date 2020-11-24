spl :: ([a], [a]) -> Integer -> ([a],[a])
spl l 0 = l
spl (xs, (y:ys)) acc = (spl ((y:xs), ys) (acc - 1))

