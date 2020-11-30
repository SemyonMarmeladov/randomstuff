
main :: IO ()
main = putStrLn "What is your name?" >>=
       (\_ ->  getLine >>=
       (\x -> putStrLn ("Hello, " ++ x ++ "!"))
       >> main)
