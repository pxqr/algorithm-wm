{-# LANGUAGE OverloadedStrings #-}
module Parser (parseModule) where

import Control.Applicative
import Control.Monad

import Data.Char
import qualified Data.List.Split as L
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser, (<?>), (.*>), (<*.), skipSpace, choice, endOfLine, endOfInput)

import AST
import Module

encloseP :: Parser c -> Parser b -> Parser a -> Parser a
encloseP p q a = do { p; r <- a; q; return r } 

inSpaces :: Parser a -> Parser a
inSpaces = encloseP skipSpace skipSpace

inParens :: Parser a -> Parser a
inParens p =  "(" .*> skipSpace *> p <* skipSpace <*. ")"

inBraces :: Parser a -> Parser a
inBraces = encloseP (P.char '{') (P.char '}') . inSpaces

keywords :: [String]
keywords = ["let", "in", "end", "case", "of"]

class Parsable p where
  parser :: Parser p
  
nameP :: Parser Name
nameP = do n <- (:) <$> P.letter <*> many (P.letter <|> P.digit) <?> "name" 
           when (n `elem` keywords) $ do
             fail "keyword is not valid name"
           return n



tyLitP :: Parser Name
tyLitP = (:) <$> P.satisfy isUpper <*> many P.letter

conP :: Parser Name
conP = tyLitP

arrowP :: Parser ()
arrowP = skipSpace >> P.string "->" >> skipSpace

litP :: Parser Literal
litP = choice 
       [ LitInt  <$> P.signed P.decimal
       , LitChar <$> (P.char '\'' *> P.anyChar <* P.char '\'')
       ]

patP :: Parser Pat
patP = choice
       [ WildP <$ P.char '_'
       , LitP  <$> litP
       , VarP  <$> nameP
       , ConP  <$> conP <*> (skipSpace *> (nameP `P.sepBy` skipSpace))
       , inParens patP
       ]

altP :: Parser Alt
altP = (,) <$> patP <*> (inSpaces (P.string "=>") *> expP)

altsP :: Parser [Alt]
altsP = inBraces (altP `P.sepBy1` inSpaces (P.char ';'))

expP :: Parser Exp
expP = foldApp <$> (lex `P.sepBy1` skipSpace) <?> "expr"
    where
      lex = choice 
            [ Bot <$  P.string "_|_"
            , Lit <$> litP
            , Let <$> ("let" .*> inSpaces nameP)
                  <*> ("="   .*> inSpaces expP)
                  <*> ("in"  .*> inSpaces expP <*. "end")
            , Case <$> ("case" .*> inSpaces expP)
                   <*> ("of"   .*> skipSpace *> altsP)
            , Var <$> nameP
            , Abs <$> ("\\" .*> inSpaces nameP  <*. ".")
                  <*> (skipSpace *> expP)
            , inParens expP
            ] 

      foldApp = foldl1 App

tyP :: Parser Ty
tyP = foldArr <$> (lex `P.sepBy1` skipSpace) <?> "type"
    where
      lex = choice 
            [ litArr <$ P.string "->"
            , LitT   <$> tyLitP
            , VarT   <$> nameP
            , inParens tyP
            ]

      foldArr = foldr1 (.->) . map (foldl1 AppT) . L.splitWhen isArr

      isArr (LitT "->") = True
      isArr _           = False

      litArr = LitT "->"
      
schemeP :: Parser (Scheme Ty)
schemeP = choice 
          [ Poly <$> ("forall" .*> inSpaces nameP <*. ".") 
                 <*> (skipSpace *> schemeP)
          , Mono <$> tyP
          ] <?> "type scheme"

kindP :: Bool -> Parser Kind
kindP lft = choice 
        [ do when lft $ do fail "left" 
             ArrK <$> kindP True <*> (arrowP *> kindP False)
        , Star <$  P.char '*'
        , inParens (kindP False)
        ] <?> "kind"

decP :: Parser Dec
decP = choice
       [ DataD <$> ("data" .*> inSpaces nameP <*. colon) 
               <*> (skipSpace *> kindP False)
       , SigD  <$> (nameP <* inSpaces ":")
               <*> schemeP
       , FunD  <$> (nameP <* skipSpace)
               <*> (nameP `P.sepBy` skipSpace)
               <*> (inSpaces "=" *> expP)
       ] <?> "declaration"
    where
      colon = ":"

moduleP :: Parser Module
moduleP = Module <$> many (decP <* (skipSpace >> P.char ';' >> skipSpace)) 
                 <* endOfInput

parseModule :: Text  -> Either String Module
parseModule = P.parseOnly moduleP