{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module AST ( Literal(..), Exp(..), Pat(..), Alt, Ty(..), Kind(..), Scheme(..)
           , Name, Subst, Term(..)
           , (.->)
           , generalize, renameScheme
           ) where

import Data.List
import Data.Char
import Data.Monoid
import qualified Data.Set as S
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Name

data Literal = LitInt  Int
             | LitChar Char
             | LitCon  Name
               deriving (Show, Eq)

data Exp = Bot
         | Lit Literal
         | Var Name
         | App Exp Exp
         | Abs Name Exp
         | Let Name Exp Exp
         | Ann Exp Ty
         | Case Exp [(Pat, Exp)]
           deriving Show

data Pat = WildP
         | LitP Literal
         | VarP Name
         | ConP Name [Name]
           deriving Show

type Alt = (Pat, Exp)

data Ty = LitT Name
        | VarT Name
        | AppT Ty Ty
        | AbsT Name Ty
          deriving (Show, Eq)

data Kind = Star
          | VarK Name
          | ArrK Kind Kind
            deriving (Eq, Show)

data Scheme a = Mono a
              | Poly Name (Scheme a)
                deriving Show

arrowT :: Ty
arrowT = LitT "->"

(.->) :: Ty -> Ty -> Ty
t1 .-> t2 = AppT (AppT arrowT t1) t2

infixr 5 .->

type Subst t = [(Name, t)]

class Term t t' | t -> t' where
  var :: Name -> t
  freeVars :: t -> Set Name
  subst :: t -> Subst t' -> t


instance Term Exp Exp where
  var   = Var
  freeVars (Var n) = S.singleton n
  freeVars (App e1 e2) = freeVars e1 <> freeVars e2
  freeVars (Abs n  e ) = n `S.delete` freeVars e
  freeVars (Let n e1 e2) = freeVars e1 <> freeVars (Abs n e2)

  subst = error "Term Exp Exp"


instance Term Ty Ty where
  var = VarT
  freeVars (LitT _) = S.empty
  freeVars (VarT n) = S.singleton n
  freeVars (AppT t1 t2) = freeVars t1 <> freeVars t2

  subst t@(LitT _)     _ = t
  subst t@(VarT n)     s | Just t' <- lookup n s = t'
                         |       otherwise       = t
  subst   (AppT t1 t2) s = AppT (subst t1 s) (subst t2 s)

instance Term Kind Kind where
  var = VarK
  freeVars  Star    = S.empty
  freeVars (VarK n) = S.singleton n
  freeVars (ArrK k1 k2) = freeVars k1 <> freeVars k2

  subst Star _ = Star
  subst k@(VarK n)     s | Just k' <- lookup n s = k'
                         |       otherwise       = k
  subst   (ArrK k1 k2) s = ArrK (subst k1 s) (subst k2 s)

instance Term a a => Term (Scheme a) a where
--  var = Mono . var
  freeVars (Mono t)   = freeVars t
  freeVars (Poly n p) = n `S.delete` freeVars p

  subst (Mono t) s = Mono (subst t s)
  subst (Poly n t) s = Poly n (subst t (filter ((n ==) . fst) s))

instance Pretty Literal where
    pretty (LitInt i)  = cyan (int i)
    pretty (LitChar c) = cyan (char '\'' <> char c <> char '\'')
    pretty (LitCon  n) = blue (text n)

instance Pretty Pat where
    pretty WildP    = red "_"
    pretty (LitP l) = pretty l
    pretty (VarP n) = text n
    pretty (ConP n ns) = blue (pretty n) <+> hsep (map text ns)

instance Pretty Exp where
  pretty = hsep . map pp . unfoldApp
      where
        unfoldApp (App e1 e2) = unfoldApp e1 ++ [e2]
        unfoldApp t = [t]

        pp    Bot        = pretty (onred "_|_")
        pp   (Lit l)     = pretty l
        pp   (Var n)     = text n
        pp t@(App e1 e2) = parens (pretty t)
        pp   (Abs n e)   = parens (lambda <> text n <> dot <+> pretty e)
            where
              lambda = green (char 'λ')

        pp (Let n e1 e2) = align $
                let' <+> text n <+> equals <+> pretty e1 </>
                in' </>
                     indent 2 (pretty e2) <$$>
                end'
            where
              [let', in', end'] = map (yellow . text) ["let", "in", "end"]

        pp (Case e1 alts) = align $
           case' <+> pretty e1 <+> of' <+> lbrace <> line <>
               indent 4 (vsep (map ppAlt alts)) <> line <>
           rbrace
            where
              [case', of'] = map yellow ["case", "of"]
              ppAlt (p, e) = pretty p <+> blue "=>" <+> pretty e

        pp (Ann e t) = parens (pretty e <+> colon <+> pretty t)

instance Pretty Ty where
  pretty = hsep . intersperse arrow . map (hsep . map pp . unfoldApp) . unfoldArr
    where
      unfoldArr (AppT (AppT (LitT "->") t1) t2) = t1 : unfoldArr t2
      unfoldArr t = [t]

      unfoldApp t@(AppT (AppT (LitT "->") _) _) = [t]
      unfoldApp (AppT t1 t2) = unfoldApp t1 ++ [t2]
      unfoldApp t = [t]

      pp (LitT n) = magenta (text n)
      pp (VarT n) = text n
      pp t = parens (pretty t)
      arrow = blue (text "->")

instance Pretty a => Pretty (Scheme a) where
    pretty (Mono t) = pretty t
    pretty (Poly n p) = forall' <> text n <> dot <+> pretty p
        where
          forall' = blue (char '∀')

instance Pretty Kind where
    pretty = hsep . intersperse arrow . map pp . unfoldArr
        where
          unfoldArr (ArrK k1 k2) = k1 : unfoldArr k2
          unfoldArr k = [k]

          pp Star = green "*"
          pp (VarK n) = blue (text n)
          pp t    = parens (pretty t)
          arrow   = green (text "->")


generalize :: Term a Ty => a -> Set Name -> Scheme a
generalize t m = S.fold Poly (Mono t) (freeVars t `S.difference` m)

renameScheme :: Term a Ty => Scheme a -> Scheme a
renameScheme = go [] (names ++ err)
    where
      go s _ (Mono ty) = Mono (subst ty s)
      go s (x : xs) (Poly n t) = Poly x (go ((n, VarT x) : s) xs t)

      names = map return ['a'..'z']
      err = error ("more than " ++ show (length names) ++ "quanifiers? Really?")
