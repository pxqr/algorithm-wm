module Parser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Identity
import Control.Monad.State as M
import Data.Char
import Data.Maybe
import Data.List as L
import Data.Map as M
import Text.Parsec as P
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Indent

import Module
import AST
import Name

strKeywords :: [String]
strKeywords = [ "let",  "in", "end"
              , "case", "of", "end"
              , "data"
              ]

opKeywords :: [String]
opKeywords = [".", "=", ":", "\\", "_"]

operatorChars :: String
operatorChars = "+-*/<>="

type PEnv = Map Name SourcePos
type St = StateT SourcePos Identity

type LangDef   = GenLanguageDef String PEnv St
type TokParser = GenTokenParser String PEnv St
type OpTable a = OperatorTable  String PEnv St a

type Parser a = ParsecT String PEnv St a


symDef :: LangDef
symDef = emptyDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , identStart      = letter
  , identLetter     = alphaNum
  , opStart         = oneOf (nub operatorChars)
  , opLetter        = oneOf (nub operatorChars)
  , reservedOpNames = opKeywords
  , reservedNames   = strKeywords
  }

tok :: TokParser
tok =  makeTokenParser symDef

sym :: String -> Parser ()
sym = reserved tok

op :: String -> Parser ()
op = reservedOp tok


nameP :: Parser Name
nameP = identifier tok

boundedName :: Parser Name
boundedName = do
  n   <- identifier tok
  env <- getState
  when (isNothing (M.lookup n env)) $ do
    fail $ "Not in scope: " ++ n ++ "\n" ++
           "Did you mean TODO"
  return n

newName :: Parser Name
newName = do
  pos <- getPosition
  n   <- identifier tok
  env <- getState
  case M.lookup n env of
    Just npos -> fail $ "Already defined at: " ++ show npos
    Nothing -> do
      setState (M.insert n pos env)
      return n

newOp :: Parser Name
newOp = parens tok (some (oneOf operatorChars))


class Expr a where
  expr  :: Parser a

exprPrec :: Expr a => String -> OpTable a -> [Parser a] -> Parser a
exprPrec msg ops atoms = buildExpressionParser ops
    (choice (fmap try (atoms ++ [parens tok expr]))) <?> msg

blockPrec msg bs = block (choice (fmap try bs)) <?> msg

conP :: Parser Name
conP = tyLitP

instance Expr Literal where
  expr = exprPrec "literal" []
       [ LitInt  <$> (fromIntegral <$> integer tok)
       , LitChar <$> charLiteral tok
       , LitCon  <$> conP
       ]


patVarP :: Parser Name
patVarP = tyVarP

patConP :: Parser Name
patConP = tyLitP


instance Expr Pat where
  expr = exprPrec "pattern" []
    [ WildP <$  sym "_"
    , ConP  <$> patConP <*> some patVarP
    , LitP  <$> expr
    , VarP  <$> patVarP
    ]

altP :: Parser Alt
altP = (,) <$> (expr <* sym "->") <*> expr

varP :: Parser Name
varP = tyVarP

instance Expr Exp where
  expr = fmap (L.foldl1 App) $ some $ indented >> exprPrec "expression" []
    [ Bot  <$  sym "_|_"
    , Lit  <$> expr
    , Var  <$> varP
    , Let  <$> (sym "let" *> nameP)
           <*> (sym "="   *> expr)
           <*> (sym "in"  *> expr <* reserved tok "end")
    , Case <$> (sym "case" *> expr)
           <*> (sym "of"   *> block altP)
    , Abs  <$> (sym "\\"   *> nameP)
           <*> (sym "."    *> expr)
    ]

tyLitP :: Parser Name
tyLitP = do
  n <- nameP
  when (isLower (head n)) $ fail "not a ty lit"
  return n

tyVarP :: Parser Name
tyVarP = do
  n <- nameP
  when (isUpper (head n)) $ fail "not a ty var"
  return n

modNameP :: Parser ModName
modNameP = ModName <$> nameP

instance Expr Ty where
  expr = indented >> exprPrec "type"
      [ [Infix (op "->" >> return (.->)) AssocRight] ]
      [apps]

   where
    apps = fmap (L.foldl1 AppT) $ some $ indented >> exprPrec "type" []
           [ LitT <$> tyLitP
           , VarT <$> tyVarP
           ]

instance Expr Kind where
  expr = exprPrec "kind"
    [ [Infix (op "->" >> return ArrK) AssocRight]
    ]
    [ Star <$  sym "*"
    , VarK <$> nameP
    ]

instance Expr a => Expr (Scheme a) where
  expr = exprPrec "scheme"    []
    [ Poly <$> (sym "forall" *> nameP <* sym ".") <*> expr
    , Mono <$> expr
    ]

instance Expr Dec where
  expr = exprPrec "declaration" []
    [ SigD  <$> (nameP <*  sym ":") <*> schemeP
    , FunD  <$>  nameP <*> many nameP  <*> (sym "=" *> expr)
    , DataD <$> (sym "data" *> nameP)
            <*> (sym ":"    *> expr)
            <*> (try (sym "where" *> block conP) <|> pure [])

    ]
   where
     conP = (,) <$> (try nameP <|> newOp) <*> (sym ":" *> expr)

instance Expr Module where
  expr = exprPrec "module" []
         [ Module <$> (sym "module" *> modNameP <* sym "where")
                  <*> block expr
         ]

tyP :: Parser Ty
tyP = expr

kindP :: Parser Kind
kindP = expr

schemeP :: Parser (Scheme Ty)
schemeP = expr

moduleP :: Parser Module
moduleP = expr

fileP :: Parser Module
fileP = whiteSpace tok *> moduleP <* eof

parseFile :: String -> IO (Either ParseError Module)
parseFile path = do
  src <- readFile path
  return (runIndent path (runParserT fileP M.empty path src))
