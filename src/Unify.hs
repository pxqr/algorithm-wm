{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Unify (UnifierM, Unifier, UFailure, runUnifier
             , reify, reduce, unify, unifyKd, isUnifiable
             , (||=), (*||=)
             ) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Set as S
import Text.PrettyPrint.ANSI.Leijen

import AST

type UTrace = [(Ty, Ty)]

data UFailure t = LitMismatchE Name Name UTrace
                | OccChkFailE  Name t    UTrace
                | UFailureE    t    t    UTrace
                | UStrMsgE     String
                  deriving Show

instance Error (UFailure t) where
  noMsg  = UStrMsgE "unknown unification failure"
  strMsg = UStrMsgE

ppCxt :: UTrace -> Doc
ppCxt = vsep . map (\(t1, t2) -> red "while trying to unify" <+> align (pretty t1 <+> red "with" </> pretty t2))

instance Pretty t => Pretty (UFailure t) where
  pretty (LitMismatchE n1 n2 cxt) =
    red (text "lit mismatch: ") <+> pretty n1 <+> "/=" <+> pretty n2 </> ppCxt cxt

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

runUnifier :: Monad m => StateT [s] m a -> m a
runUnifier u = evalStateT u []


bindVar :: Name -> t -> Unifier t ()
bindVar n t = modify ((n, t) :)

withBind :: Name -> t -> Unifier t a -> Unifier t a
withBind n t a = do
  s <- get
  bindVar n t
  r <- a
  put s
  return r


freshVars :: [Ty]
freshVars = map mkVar [0 :: Integer ..]
  where
    mkVar i = var ("_v" ++ show i)


reify :: Eq t => Term t t => t -> Unifier t t
reify = gets . go
  where
    go t s = let t' = subst t s in
      if t == t' then t else go t' s


--class Unifiable u where
--  unify :: u -> u -> Unifier u ()

reduce :: Ty -> Ty
reduce = go []
  where
    go _ (LitT n)     = (LitT n)
    go s (VarT n)     | Just t <- lookup n s = t
                      | otherwise = VarT n
    go s (AppT t1 t2) =
      let t1' = go s t1
          t2' = go s t2
      in case t1' of
        AbsT n t -> go ((n, t2') : s) t
        _        -> AppT t1' t2'

    go s (AppTE t e) = AppTE (go s t) e

    go s (AbsT n t)   = AbsT n (go ((n, var n) : s) t)

unify :: Ty -> Ty -> Unifier Ty ()
unify _t1 _t2 =
  let _t1' = reduce _t1
      _t2' = reduce _t2
  in go [(_t1', _t2'), (_t1, _t2)] freshVars _t1' _t2'
    where
      go _   _   (LitT n)     (LitT m)     | n == m = return ()
      go _   _   (VarT a)     (VarT b)     | a == b = return ()
      go cxt _   t            (VarT n)     = assignVar cxt n t
      go cxt _   (VarT n)     t            = assignVar cxt n t

      go cxt fvs (AppT t1 t2) (AppT t3 t4) = do
        go ((t1, t3) : cxt) fvs t1 t3
        t2' <- reify t2
        t4' <- reify t4
        go ((t2', t4') : cxt) fvs t2' t4'

      go cxt fvs (AppTE t1 e1) (AppTE t2 e2) = do
        go ((t1, t2) : cxt) fvs t1 t2


      go cxt fvs (AbsT n1 t1) (AbsT n2 t2) = do
        withBind n1 (var n1) $ do
          withBind n2 (var n1) $ do
            go ((t1, t2) : cxt) fvs t1 t2

      go cxt _   t1           t2 = throwError (UFailureE t1 t2 cxt)

      assignVar :: UTrace -> Name -> Ty -> Unifier Ty ()
      assignVar _ n (VarT n')
        | n == n'   = return ()
      assignVar cxt n t
        | n `S.member` freeVars t = throwError (OccChkFailE n t cxt)
        |       otherwise         = reify t >>= bindVar n


eqTy :: Ty -> Ty -> Unifier Ty Bool
eqTy = eq freshVars []
  where
    eq :: [Ty] -> Subst Ty -> Ty -> Ty -> Unifier Ty Bool
    eq _   _ (LitT n)     (LitT m) = return (n == m)
    eq _   s a@(VarT _) b@(VarT _) = do
      a' <- reify (subst a s)
      b' <- reify (subst b s)
      return (a' == b')

    eq fvs s (AppT t1 t2) (AppT t3 t4) = do
      a <- eq fvs s t1 t3
      b <- eq fvs s t2 t4
      return (a && b)

    eq fvs s (AppTE t1 e1) (AppTE t2 e2) = do
      eq fvs s t1 t2

    eq (fv : fvs) s (AbsT n1 t1) (AbsT n2 t2) =
      eq fvs ((n1, fv) : (n2, fv) : s) t1 t2

    eq _   _ _        _        = return False

(||=) :: Ty -> Ty -> Unifier Ty Bool
t1 ||= t2 = do
  unify t2 t1
  t1' <- reify  (reduce t1)
  t2' <- return (reduce t2)
  eqTy t1' t2'


unifyKd :: Kind -> Kind -> Unifier Kind ()
unifyKd = go
  where
    go  Star     Star    = return ()
    go (DatK n) (DatK m) | n == m = return ()
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

isUnifiable :: Kind -> Kind -> Bool
isUnifiable a b = case runUnifier (unifyKd a b) of
  Right _ -> True
  _       -> False


eqKind :: Kind -> Kind -> Bool
eqKind  = (==)

(*||=) :: Kind -> Kind -> Unifier Kind Bool
k1 *||= k2 = do
  unifyKd k1 k2
  k1' <- reify k1
  return (eqKind k1' k2)
