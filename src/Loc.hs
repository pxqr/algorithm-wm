module Loc where

import Name

data Loc = Loc {
    line :: Int     -- number of lines
  , col  :: Int     -- number of columns
  , place :: Name   -- in which definition
  , modName :: Name -- 
  }

class Location Loc where
  
data ParseError = P

type Result = Either ParseError

-- 
data Lex = 
  
  
data PState a = (Text, Loc, [Parser])
data Pars a b = StateT (PState a) (Either PraseError)
-- main parser
type Lexer  = Pars Text Lexeme -- recognize layout, remove space
type Parser = Pars Lexeme AST  -- parse syntax, rule engine

instance Category Pars where
  id = undefined
   .