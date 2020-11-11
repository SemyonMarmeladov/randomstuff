
-- Implementing the Monadic Parser Combinators by
-- Miejer and Hutton


{-# OPTIONS_GHC -fdefer-type-errors #-}
module Parser where

-- s-expr parser
import Control.Applicative
import Data.Char

-- A parser takes a string and returns something and a string.

newtype Parser a = P (String -> [(a, String)])
data Primitives =
               Op Char
               | Num Int
parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item  = P(\s -> case s of
             [] -> []
             (x : xs) -> [(x, xs)])

instance Functor Parser where
  fmap f fa = P (\s -> case parse fa s of
                    []                 -> []
                    [(first, out)]     -> [((f first), out)]
                    ((first, out):_)   -> [((f first), out)])


instance Applicative Parser where
  pure p =  P (\s -> [(p, s)])
   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\s -> case parse pg s of
                    []             -> []
                    [(a, rest)]    -> parse (fmap a px) rest)


instance Monad Parser where
  -- m a -> (a -> m b) -> m b
  (>>=) ma amb = P (\s -> case parse ma s of
                     []          -> []
                     [(a, rest)] -> parse (amb a) rest)


instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\_ -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(a,rest)] -> [(a,rest )])


-- Parser combinators

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\x ->
           if p x then return x else empty)

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

openParen :: Parser Char
openParen = char '('

plus :: Parser Char
plus = char '+'

parenOp :: Parser Char
parenOp = openParen >>= (\x -> plus)

-- Will revisit how to make a ast once i figure out readerT or wahtever
