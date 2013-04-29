{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}
module AST ( Exp(..), Pat(..), Alt, Ty(..), Kind(..), Scheme(..)
           , Name, Subst, Term(..)
           , (.->), isFunc
           , generalize, renameScheme, instQs
           ) where

import Data.List as L
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Name

data Exp = Bot
         | ConE Name
         | Var Name
         | App Exp Exp
         | Abs Name Exp
         | Let Name Exp Exp
         | Ann Exp Ty
         | Case Exp [(Pat, Exp)]
           deriving Show

data Pat = WildP
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

isFunc :: Ty -> Bool
isFunc (AppT (AppT (LitT "->") _) _) = True
isFunc _                             = False

type Subst t = [(Name, t)]

class Term t t' | t -> t' where
  var :: Name -> t
  freeVars :: t -> Set Name
  subst :: t -> Subst t' -> t

instance Term Name Name where
  var = id
  freeVars = S.singleton
  subst n s = fromMaybe n (L.lookup n s)

instance Term Pat Name where
  var = VarP

  freeVars  WildP      = S.empty
  freeVars (VarP n)    = S.singleton n
  freeVars (ConP _ fs) = S.fromList fs

  subst  WildP      _ = WildP
  subst (VarP n)    s = VarP (subst n s)
  subst (ConP n fs) s = ConP n (map (`subst`  s) fs)


instance Term Exp Exp where
  var   = Var
  freeVars  Bot    = S.empty
  freeVars (Var n) = S.singleton n
  freeVars (ConE _) = S.empty
  freeVars (App e1 e2) = freeVars e1 <> freeVars e2
  freeVars (Abs n  e ) = n `S.delete` freeVars e
  freeVars (Let n e1 e2) = freeVars e1 <> freeVars (Abs n e2)
  freeVars (Ann e _)     = freeVars e
  freeVars (Case e as)   = freeVars e <> mconcat (map fvAlt as)
     where
       fvAlt (p, ex) = freeVars ex `S.difference` freeVars p

  subst = error "Term Exp Exp"



instance Term Ty Ty where
  var = VarT

  freeVars (LitT _)     = S.empty
  freeVars (VarT n)     = S.singleton n
  freeVars (AppT t1 t2) = freeVars t1 <> freeVars t2
  freeVars (AbsT n t)   = S.delete n (freeVars t )

  subst t@(LitT _)     _ = t
  subst t@(VarT n)     s | Just t' <- lookup n s = t'
                         |       otherwise       = t
  subst   (AppT t1 t2) s = AppT (subst t1 s) (subst t2 s)
  subst   (AbsT n t)   s = subst t ((n, var n) : s)
    -- WARN or maybe delete _n_ from _s_?

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
  var n = Mono (var n)

  freeVars (Mono t)   = freeVars t
  freeVars (Poly n p) = n `S.delete` freeVars p

  subst (Mono t) s = Mono (subst t s)
  subst (Poly n t) s = Poly n (subst t (filter ((n ==) . fst) s))

instance Pretty Pat where
    pretty WildP    = red "_"
    pretty (VarP n) = text n
    pretty (ConP n ns) = blue (pretty n) <+> hsep (map text ns)

toNat :: Exp -> Maybe Int
toNat (ConE "Z")         = return 0
toNat (App (ConE "S") n) = succ `fmap` toNat n
toNat _                  = Nothing

toList :: Exp -> Maybe [Exp]
toList (ConE "Nil")                   = return []
toList (App (App (ConE "Cons") x) xs) = (x :) `fmap` toList xs
toList _                              = Nothing

toPair :: Exp -> Maybe (Exp, Exp)
toPair (App (App (ConE "MkPair") a) b) = return (a, b)
toPair _                               = Nothing

isUnitExp :: Exp -> Bool
isUnitExp (ConE "MkUnit") = True
isUnitExp _               = False

instance Pretty Exp where
  pretty ex | isUnitExp ex = blue "()"
  pretty ex | Just i <- toNat ex  = pretty (int i)
  pretty ex | Just l <- toList ex = pretty (map pretty l)
  pretty ex | Just (a, b) <- toPair ex = parens (pretty a <> comma <+> pretty b)
  pretty ex = hsep (map pp (unfoldApp ex))
      where
        unfoldApp (App e1 e2) = unfoldApp e1 ++ [e2]
        unfoldApp t = [t]

        pp    Bot        = pretty (onred "_|_")
        pp   (Var n)     = text n
        pp   (ConE n)    = blue (text n)
        pp t@(App _ _)   = parens (pretty t)
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


isUnitTy :: Ty -> Bool
isUnitTy (LitT "Unit") = True
isUnitTy _             = False

toPairTy :: Ty -> Maybe (Ty, Ty)
toPairTy (AppT (AppT (LitT "Pair") a) b) = Just (a, b)
toPairTy _                               = Nothing

ppPairTy :: (Ty, Ty) -> Doc
ppPairTy (a, b) = blue "("
                   <> pretty a <> blue comma <+> pretty b
               <> blue ")"

toListTy :: Ty -> Maybe Ty
toListTy (AppT (LitT "List") a) = return a
toListTy _                      = Nothing

ppListTy :: Ty -> Doc
ppListTy a = blue "[" <> pretty a <> blue "]"

instance Pretty Ty where
  pretty ty
   | isUnitTy ty = blue "()"
   | Just a <- toListTy ty = ppListTy a
   | Just prty <- toPairTy ty = ppPairTy prty
   | otherwise = hsep $ intersperse arrow $
       map (hsep . map pp . unfoldApp) $ unfoldArr ty
    where
      unfoldArr (AppT (AppT (LitT "->") t1) t2) = t1 : unfoldArr t2
      unfoldArr t = [t]

      unfoldApp t@(AppT (AppT (LitT "->") _) _) = [t]
      unfoldApp t | Just _ <- toListTy t = [t]
      unfoldApp t | Just _ <- toPairTy t = [t]
      unfoldApp (AppT t1 t2) = unfoldApp t1 ++ [t2]
      unfoldApp t = [t]

      pp t | Just a <- toListTy t = ppListTy a
      pp t | Just a <- toPairTy t = ppPairTy a
      pp (LitT n) = magenta (text n)
      pp (VarT n) = text n
      pp (AbsT n t) = parens (lambda <> text n <> dot
                                     <+> pretty t)
        where
          lambda = green (char 'λ')

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

instQs :: Scheme a -> a
instQs (Mono t) = t
instQs (Poly _ t) = instQs t

generalize :: Term a a => a -> Set Name -> Scheme a
generalize t m = S.fold Poly (Mono t) (freeVars t `S.difference` m)

renameScheme :: Term a Ty => Scheme a -> Scheme a
renameScheme = go [] (names ++ err)
    where
      go s _ (Mono ty) = Mono (subst ty s)
      go s (x : xs) (Poly n t) = Poly x (go ((n, VarT x) : s) xs t)
      go _ []       (Poly _ _) = err

      names = map return ['a'..'z']

      err :: forall a. a
      err = error ("more than " ++ show (length names) ++ "quanifiers? Really?")
