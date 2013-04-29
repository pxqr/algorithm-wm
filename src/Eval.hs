{-# LANGUAGE OverloadedStrings #-}
module Eval (evalName, Value) where

import Control.Applicative
import Data.Maybe
import Data.Char
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

instance Pretty Value where
  pretty = pretty . valueToExp
    where
      valueToExp :: Value -> Exp
      valueToExp = go ['a'..'z']
        where
          go _ (BotV)   = Bot
          go _ (LitV l) = Lit l
--      go _ v@(ConV "Cons" (LitV (LitChar _) : _)) = Lit $ LitStr (foldStr v)
--        where
--          foldStr :: Value -> String
--          foldStr (ConV "Cons" (LitV (LitChar x) : [xs])) = x : foldStr xs
--          foldStr (ConV "Nil" []) = ""
--          foldStr _ = error "valueToExp.foldStr: not typecheck expr"

--      go _ v@(ConV "Z" []) = Lit (LitInt 0)
--      go _ v@(ConV "S" [n]) = Lit (LitInt (succ (foldNat n)))
--        where
--          foldNat (ConV "Z" []) = 0
--          foldNat (ConV "S" [x]) = foldNat x
--          foldNat _              = error "value to expr"

          go ns (ConV n fs) = foldl App (Lit (LitCon n)) (map (go ns) fs)
          go (n : ns) (AbsV f) = Abs [n] (go ns (f (LitV (LitSym [n]))))
          go []   _     = error "value to expr"



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
        | Just ix <- n `M.lookup`  globSym  =
          if isUpper (head n) then error "toExpC" else Free ix
        |             otherwise             = error ("link: unbound symbol " ++ n)

    toExpC local (Abs n e)     = AbsC (toExpC (n : local) e)
    toExpC local (App e1 e2)   = AppC (toExpC local e1) (toExpC local e2)
    toExpC local (Let n e1 e2) = toExpC local (App (Abs n e2) e1)
    toExpC local (Case e alts) = CaseC (toExpC local e) (map mkAlt alts)
        where
          mkAlt (p, e') = (p, bindPat p e')

          bindPat  WildP      e' = toExpC local e'
          bindPat (LitP _)    e' = toExpC local e'
          bindPat (VarP n)    e' = toExpC local (Abs n e')
          bindPat (ConP _ ns) e' = toExpC local (foldr Abs e' ns)

    toExpC local (Ann e _)     = toExpC local e

apply :: Value -> Value -> Value
apply  BotV    _ = BotV
apply (AbsV f) v = f v
apply (ConV n fs) v = ConV n (fs ++ [v])
apply v1       a = error $ "Unable to apply " ++ show (pretty v1) ++ " "
                        ++ "to " ++ show (pretty a)

eval :: EvEnv -> Stack -> ExpC -> Value
eval _   _  BotC       = BotV
eval _   _ (LitC (LitCon n)) = ConV n []
eval _   _ (LitC l)    = LitV l
eval env s (Free ix)   = eval env s (V.unsafeIndex env ix)
eval _   s (Bnd  ix)   = s !! ix
eval env s (AbsC e)    = AbsV (\x -> eval env (x : s) e)
eval env s (AppC e1 e2) = apply (eval env s e1) (eval env s e2)
eval env s (CaseC e1   alts) = case eval env s e1 of
                                 BotV -> BotV
                                 v    -> select v alts
    where
      select :: Value -> [AltC] -> Value
      select v ((p, e) : xs) | Just bs <- match v p = foldr (flip apply) (eval env s e) $
                                                      reverse bs
                             | otherwise = select v xs
      select _ [] = errNonExhaustive

      match :: Value -> Pat -> Maybe [Value]
      match _              WildP        = Just []
      match (LitV v)      (LitP l)      | v `matchLit` l = Just []  where matchLit = (==)
      match v             (VarP _)      = Just [v]
      match (ConV en evs) (ConP pn pns) | en == pn =
           if length evs == length pns then Just evs
           else error ("EVAL: Con arity mismatch " ++ show (pretty evs) ++ " " ++ show pns)
      match _             _             = Nothing

      errNonExhaustive = error ("Non-exhaustive patterns\n"
                                ++ show (pretty (CaseC e1 alts)) ++ "\n"
                                ++ show (alts) ++ "\n"
                                ++ show (map pretty s) ++ "\n"
                                ++ show (pretty (eval env s e1))
                               )


evalName :: Name -> Module -> Maybe Value
evalName entryName m = eval prg [] . (prg V.!) <$> (entryName `elemIndex`  (map fst defs))
    where
      prg :: EvEnv
      prg = link defs

      defs = mapMaybe funD (modDecs m)

      funD :: Dec -> Maybe (Name, Exp)
      funD (FunD n ps e) = Just (n, desugar e ps)
      funD _             = Nothing

      desugar = foldr Abs
