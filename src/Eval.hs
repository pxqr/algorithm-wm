{-# LANGUAGE OverloadedStrings #-}
module Eval (evalMain, Value) where

import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import AST
import Module

type GlIx = Int
type LcIx = Int

data Prim = Add | Sub 

type AltC = (Pat, ExpC)

data ExpC = BotC
          | LitC  Literal
          | Free  {-# UNPACK #-} !GlIx
          | Bnd   {-# UNPACK #-} !LcIx
          | AbsC  ExpC
          | AppC  ExpC ExpC
          | CaseC ExpC [AltC]
            deriving Show

data Value = BotV
           | LitV Literal
           | ConV Name [Value]
           | AbsV (Value -> Value)

instance Pretty ExpC where
    pretty BotC = onred "_|_"
    pretty (LitC l) = pretty l
    pretty (Free i) = magenta (int i)
    pretty (Bnd  i) = blue    (pretty i)
    pretty (AbsC e) = parens (char '\\' <> pretty e)
    pretty (AppC e1 e2) = parens (pretty e1 <+> pretty e2)
    pretty (CaseC e1 alts) = "case" <+> pretty e1 <+> "of" <> line <>
                                    indent 4 (vcat (map ppAlt alts))
                                    where 
                                      ppAlt (p, e) = pretty p <+> "=>" <+> pretty e

valueToExp :: Value -> Exp
valueToExp = go ['a'..'z']
    where
      go _ (BotV)   = Bot
      go _ (LitV l) = Lit l
      go _ (ConV n _) = error "value to expr"
      go (n : ns) (AbsV f) = Abs [n] (go ns (f (LitV (LitChar n))))
      go []   _     = error "value to expr"

instance Pretty Value where
    pretty = pretty . valueToExp


type Stack = [Value]
type EvEnv = Vector ExpC

link :: [(Name, Exp)] -> EvEnv
link defs = V.fromList (map (toExpC [] . snd) defs)
  where
    globSym :: Map Name GlIx
    globSym = M.fromList (zip (map fst defs) [0..])

    toExpC :: [Name] -> Exp -> ExpC
    toExpC _      Bot    = BotC
    toExpC _     (Lit l) = LitC l
    toExpC localSym (Var n) 
        | Just ix <- n `elemIndex` localSym = Bnd ix
        | Just ix <- n `M.lookup`  globSym  = Free ix
        |             otherwise             = error ("link: unbound symbol " ++ n)

    toExpC local (Abs n e)     = AbsC (toExpC (n : local) e)
    toExpC local (App e1 e2)   = AppC (toExpC local e1) (toExpC local e2)
    toExpC local (Let n e1 e2) = toExpC local (App (Abs n e2) e1)
    toExpC local (Case e alts) = CaseC (toExpC local e) (map mkAlt alts)
        where
          mkAlt (p, e) = (p, bindPat p e)

          bindPat  WildP      e = toExpC local e
          bindPat (LitP _)    e = toExpC local e
          bindPat (VarP n)    e = toExpC local (Abs n e)
          bindPat (ConP n ns) e = toExpC local (foldr Abs e ns)

    toExpC local (Ann e _)     = toExpC local e

apply :: Value -> Value -> Value
apply  BotV    _ = BotV
apply (AbsV f) v = f v
apply v1       a = error ("Unable to apply " ++ show (pretty v1) ++ show (pretty a))

eval :: EvEnv -> Stack -> ExpC -> Value 
eval _   s  BotC       = BotV
eval _   s (LitC l)    = LitV l
eval env s (Free ix)   = eval env s (V.unsafeIndex env ix)
eval env s (Bnd  ix)   = s !! ix
eval env s (AbsC e)    = AbsV (\x -> eval env (x : s) e)
eval env s (AppC e1 e2) = apply (eval env s e1) (eval env s e2)
eval env s (CaseC e1   alts) = case eval env s e1 of 
                                 BotV -> BotV
                                 v    -> select v alts
    where
      select :: Value -> [AltC] -> Value
      select v ((p, e) : xs) | Just bs <- match v p = foldr (flip apply) (eval env s e) bs
                             | otherwise = select v xs
      select _ [] = errNonExhaustive

      match :: Value -> Pat -> Maybe [Value]
      match _              WildP        = Just []
      match (LitV v)      (LitP l)      | v `matchLit` l = Just []  where matchLit = (==) 
      match v             (VarP n)      = Just [v]
      match (ConV en evs) (ConP pn pns) | en == pn =
           if length evs == length pns then Just evs
           else error ("EVAL: Con arity mismatch " ++ show (pretty evs) ++ " " ++ show pns)
      match _             _             = Nothing
      
      errNonExhaustive = error ("Non-exhaustive patterns\n" ++ show (pretty (CaseC e1 alts)))



entryName :: Name
entryName = "main"

evalMain :: Module -> Maybe Value
evalMain m = eval prg [] . (prg V.!) <$> (entryName `elemIndex`  (map fst defs))
    where
      prg :: EvEnv
      prg = link defs

      defs = mapMaybe funD (modDecs m)

      funD :: Dec -> Maybe (Name, Exp)
      funD (FunD n ps e) = Just (n, desugar e ps)
      funD _             = Nothing

      desugar = foldr Abs 
