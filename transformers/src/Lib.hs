module Lib where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map


exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x"))(Lit 4 `Plus` Lit 2))

type Name = String
data Exp = Lit Integer 
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp 
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value 


