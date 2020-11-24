import Data.Char 


isPalindrome :: String -> Maybe Bool
isPalindrome "" = Nothing
isPalindrome word 
  | (word == (reverse word)) = Just True
  | otherwise = Just False 

-- they also filter some stuff whatevs.
--
verbose :: String -> String
verbose s = case (isPalindrome s) of Nothing      -> "Empty string"
                                     (Just True)  -> "It is a palindrome"
                                     (Just False) -> "It is not a palindrome"



-- Now we want to reject strings with spaces.
-- /w option type

db :: [(Integer, String)]
db = [(3, "Joe"), (4, "notJoe")]

removeSpaces :: String -> Maybe String
removeSpaces s 
 | (filter (\x -> x /= ' ') s) == s = Just s
 | otherwise = Nothing

validateLength :: String -> Maybe String
validateLength s
  | length s < 25 =  Just s
  | otherwise = Nothing

rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic s = case (all isAlpha s) of
                        True  -> Just s
                        False -> Nothing

makeUserName :: String -> Maybe String 
makeUserName s = removeSpaces s >>= validateLength >>= rejectNonAlphabetic



greetUser :: Integer -> Maybe String
greetUser record =
    fmap ("Hello, " ++) (lookup record db)

-- A semigroup is a set, and an binary operator that works on those two sets.
--
-- A monoid also has an identitiy
--
-- fold :: (t Foldable, m monoid) => t m -> 

main :: IO () 
main = do 
  word <- getLine
  print(verbose word) 
          
