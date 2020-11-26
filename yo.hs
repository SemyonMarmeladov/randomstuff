import System.Random

let gen = getStdGen

rnd_select xs n = do
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

