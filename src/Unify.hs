{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Unify (UnifierM, Unifier, UFailure
             , reify, unify, unifyKd
             , (||=)
             ) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Set as S
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen

import AST

type UTrace = [(Ty, Ty)]

data UFailure t = LitMismatchE Name Name UTrace
                | OccChkFailE  Name t    UTrace
                | UFailureE    t    t    UTrace
                | UStrMsgE     String

instance Error (UFailure t) where
  noMsg  = UStrMsgE "unknown unification failure"
  strMsg = UStrMsgE

ppCxt :: UTrace -> Doc
ppCxt = vsep . map (\(t1, t2) -> red "while trying to unify" <+> align (pretty t1 <+> red "with" </> pretty t2))

instance Pretty t => Pretty (UFailure t) where
  pretty (LitMismatchE n1 n2 cxt) =
    red (text "lit mismatch: ") <+> pretty n1 <+> "/=" <+> pretty n2

  pretty (OccChkFailE n t  cxt)  =
    red (text "occur check failed") <+> text n </>
    red (text "          occur in") <+> pretty t </>
    ppCxt cxt

  pretty (UFailureE t1 t2 cxt) =
        red (text "can't unify") <+> pretty t1 <+> red (text "with") </>
                                     pretty t2 </>
                                     ppCxt cxt

  pretty (UStrMsgE str) = red (text str)


type UResult t = Either (UFailure t)
type UnifierM t m = StateT (Subst t) m

type Unifier t = UnifierM t (UResult t)

bindVar :: Name -> t -> Unifier t ()
bindVar n t = modify ((n, t) :)

reify :: Term t t => t -> Unifier t t
reify = gets . subst

--class Unifiable u where
--  unify :: u -> u -> Unifier u ()

unify :: Ty -> Ty -> Unifier Ty ()
unify _t1 _t2 = go [(_t1, _t2)] _t1 _t2
    where
      go _   (LitT n)     (LitT n')    |  n == n'  = return ()

      go cxt t            (VarT n)     = assignVar cxt n t
      go cxt (VarT n)     t            = assignVar cxt n t

      go cxt (AppT t1 t2) (AppT t3 t4) = do
        go ((t1, t3) : cxt) t1 t3
        t2' <- reify t2
        t4' <- reify t4
        go ((t2, t4) : cxt) t2' t4'

      go cxt t1           t2 = throwError (UFailureE t1 t2 cxt)

      assignVar :: UTrace -> Name -> Ty -> Unifier Ty ()
      assignVar cxt n t
          | VarT n' <- t, n == n'   = return ()
          | n `S.member` freeVars t = throwError (OccChkFailE n t cxt)
          |       otherwise         = bindVar n t

(||=) :: Ty -> Ty -> Unifier Ty Bool
t1 ||= t2 = do
  unify t1 t2
  t1' <- reify t1
  return (t1 == t1')


unifyKd :: Kind -> Kind -> Unifier Kind ()
unifyKd = go
  where
    go Star     Star     = return ()
    go k        (VarK n) = assignVar n k
    go (VarK n) k        = assignVar n k
    go (ArrK k1 k2) (ArrK k3 k4) = do
      go k1 k3
      k2' <- reify k2
      k4' <- reify k4
      go k2' k4'

    go k1 k2 = throwError (UFailureE k1 k2 [])

    assignVar n t
      | VarK n' <- t, n == n'   = return ()
      | n `S.member` freeVars t = throwError (OccChkFailE n t [])
      |       otherwise         = bindVar n t
