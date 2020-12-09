{-# LANGUAGE OverloadedStrings #-}
module MyParser (main) where

import Text.Megaparsec
import System.Environment
import Data.Text (Text, pack)
import Data.Void

main = do
  input <- getLine 
  parseTest singleLetterP (pack input)

type Parser = Parsec Void Text

singleLetterP :: Parser Text
singleLetterP = count 2 (satisfy (== 'h'))
