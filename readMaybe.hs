import Text.Read 
main = do 
        l <- getLine
        c <- getLine
        let r = readMaybe l :: Maybe Integer
        let r = readMaybe l :: Maybe Integer
        case r of 
          (Just x) -> putStrLn ("The number is " ++ (show x)) 
          Nothing  -> do 
                      putStrLn "Not valid. "
                      main



