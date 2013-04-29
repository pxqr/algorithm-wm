module Parser
       ( parseFile

       -- * for REPL
       , nameP, expP, patP, tyP, kindP, inRepl
       ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Identity
import Control.Monad.State as M
import Data.Char
import Data.Maybe
import Data.List as L
import Data.Map as M
import Text.Parsec as P hiding (optional)
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Indent
import qualified Text.Parsec.String as S

import AST
import Module
import Name


strKeywords :: [String]
strKeywords = [ "let",  "in", "end"
              , "case", "of", "end"
              , "data"
              , "_"
              ]

opKeywords :: [String]
opKeywords = ["[", "]", ".", "=", ":", "\\", "_"]

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

blockPrec :: String -> [Parser a] -> Parser [a]
blockPrec msg bs = block (choice (fmap try bs)) <?> msg

conP :: Parser Name
conP = tyLitP

patVarP :: Parser Name
patVarP = tyVarP

patConP :: Parser Name
patConP = tyLitP

patPairP :: Parser (Name, Name)
patPairP = (,) <$> (op "(" *> patVarP)
               <*> (op "," *> patVarP <* op ")")

patPairC :: (Name, Name) -> Pat
patPairC (a, b) = ConP "MkPair" [a, b]

patNilP :: Parser Pat
patNilP = ConP "Nil" [] <$ (op "[" >> op "]")

patConsP :: Parser Pat
patConsP = constr <$> (patVarP <* op ":")
                  <*>  patVarP
  where
    constr = \a b -> ConP "Cons" [a, b]

unitPatP :: Parser Pat
unitPatP = ConP "MkUnit" [] <$ (op "(" >> op ")")

instance Expr Pat where
  expr = same >> exprPrec "pattern" []
    [ WildP <$  sym "_"
    , unitPatP
    , patPairC <$> patPairP
    , patNilP
    , patConsP
    , ConP  <$> patConP <*> many patVarP
    , VarP  <$> patVarP
    ]

altP :: Parser Alt
altP = withPos $ do
  p <- expr
  op "->"
  b <- expr
  return (p, b)

varP :: Parser Name
varP = tyVarP

numC :: Integer -> Exp
numC 0 = ConE "Z"
numC n = App (ConE "S") (numC (pred n))

listP :: Parser [Exp]
listP = op "[" *> sepBy expr (op ",") <* op "]"

listC :: [Exp] -> Exp
listC = L.foldr (App . App (ConE "Cons")) (ConE "Nil")

pairP :: Parser (Exp, Exp)
pairP = (,) <$> (op "(" *> expr <* op ",")
            <*> (expr <* op ")")

pairC :: (Exp, Exp) -> Exp
pairC (a, b) = App (App (ConE "MkPair") a) b

unitExpP :: Parser Exp
unitExpP = ConE "MkUnit" <$ (op "(" >> op ")")

consCon :: Exp -> Exp -> Exp
consCon = App . App (ConE "Cons")

instance Expr Exp where
  expr = sameOrIndented >> exprPrec "expression"
      [ [Infix (op ":" >> return consCon) AssocRight] ]
      [ term ]
    where
      term = fmap (L.foldl1 App) $ some $
             sameOrIndented >> exprPrec "expression" []
             [ Bot  <$  sym "_|_"
             , numC <$> integer tok
             , unitExpP
             , pairC <$> pairP
             , listC <$> listP
             , Var  <$> varP
             , ConE <$> conP
             , Let  <$> (sym "let" *> nameP)
                    <*> (sym "="   *> expr)
                    <*> (sym "in"  *> withPos expr <* sym "end")
             , Case <$> (sym "case" *> expr)
                    <*> (sym "of"   *> block altP)
             , Abs  <$> (op  "\\"   *> varP)
               <*> (op  "."    *> expr)
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

unitTyP :: Parser Ty
unitTyP = LitT "Unit" <$ (op "(" >> op ")")

pairTyP :: Parser Ty
pairTyP = (AppT . AppT (LitT "Pair"))
           <$> (op "(" *> expr <* op ",")
           <*> (expr <* op ")")

listTyP :: Parser Ty
listTyP = AppT (LitT "List") <$> (op "[" *> expr <* op "]")


instance Expr Ty where
  expr = exprPrec "type"
      [ [Infix (op "->" >> return (.->)) AssocRight] ]
      [apps]

   where
    apps = fmap (L.foldl1 AppT) $ some $ do
             sameOrIndented
             exprPrec "type" []
               [ LitT <$> tyLitP
               , VarT <$> tyVarP
               , AbsT <$> (op "\\" *> tyVarP)
                      <*> (op "."  *> expr)
               , unitTyP
               , pairTyP
               , listTyP
               ]

instance Expr Kind where
  expr = exprPrec "kind"
    [ [Infix (op "->" >> return ArrK) AssocRight]
    ]
    [ Star <$  op "*"
    , VarK <$> nameP
    ]

instance Expr a => Expr (Scheme a) where
  expr = exprPrec "scheme"    []
    [ Poly <$> (sym "forall" *> nameP <* op ".") <*> expr
    , Mono <$> expr
    ]

funBody :: Parser Exp
funBody = expr

instance Expr Dec where
  expr = exprPrec "declaration" []
    [ SigD  <$> (nameP <*  op ":") <*> (indented >> schemeP)
    , (FunD  <$>  nameP <*> (many nameP <* op "=")) <+/> funBody
    , DataD <$> (sym "data" *> nameP)
            <*> (op  ":"    *> (indented >> expr))
            <*> (try (sym "where" *> block consP) <|> pure [])
    ]
   where
     consP = (,) <$> (try nameP <|> newOp) <*> (op ":" *> expr)


importP :: Parser ModName
importP = sym "import" *> modNameP <?> "import"

instance Expr Module where
  expr = exprPrec "module" []
         [ Module <$> (sym "module" *> modNameP <* sym "where")
                  <*> importsP
                  <*> block expr
         ]
    where
      importsP = fromMaybe [] <$> optional (block importP)


expP :: Parser Exp
expP = expr

patP :: Parser Pat
patP = expr

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


inRepl :: Parser a -> S.Parser a
inRepl p = do
  s <- many anyChar <* eof
  let path = ":interactive:"
  let res  = runIndent path (runParserT (withPos p <* eof) M.empty path s)
  case res of
    Left e  -> fail (show e)
    Right r -> return r