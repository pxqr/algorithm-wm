{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module TC where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as S

import AST
import Unify
import TyError


data Info = HasType (Scheme Ty)
          | HasKind Kind
            deriving Show

type TyEnv = [(Name, Info)]

type Result = Either TyError

type Index = Int
type Context t = ReaderT TyEnv (StateT Index (UnifierM t Result))

generalizeM :: Term a Ty => a -> Context t (Scheme a)
generalizeM ty = generalize ty . S.fromList . map fst <$> tyEnv

substTyEnv :: TyEnv -> Subst Ty  -> TyEnv
substTyEnv env s = map substInfoItem env
    where
      substInfoItem (n, HasType ty) = (n, HasType (subst ty s))
      substInfoItem info = info

border :: Name
border = "__expr_border"; -- TODO: remove

tyEnv :: Context t TyEnv
tyEnv = ask

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right a) = Right a

withU :: Unifier Ty a -> Context Ty a
withU = lift . lift . mapStateT (mapLeft UnificationE)

withUKd :: Unifier Kind a -> Context Kind a
withUKd = lift . lift . mapStateT (mapLeft KdUnificationE)

fresh :: Context t Name
fresh = do
  i <- get
  modify succ
  return ('v' : show i)

freshVar :: Term a b => Context t a
freshVar = var <$> fresh

freshInst :: Term a a => Scheme a -> Context t a
freshInst = go []
  where
    go s (Mono t)   = return (subst t s)
    go s (Poly n t) = do
      f <- freshVar
      go ((n, f) : s) t

bindLocal :: Name -> Scheme Ty -> Context t a -> Context t a
bindLocal n t = local ((n, HasType t) :)

bindLocalMany :: Subst (Scheme Ty) -> Context t a -> Context t a
bindLocalMany bs = local (map (second HasType) bs ++)

bindKinds :: Scheme Ty -> (Ty -> Context t a) -> Context t a
bindKinds sc a = go sc
  where
    go (Mono t) = a t
    go (Poly n t) = do
      k <- freshVar
      local ((n, HasKind k) :) (go t)

substLocal :: Context Ty a -> Context Ty a
substLocal cxt = do
  s <- withU $ do get
  local (`substTyEnv` s) cxt

lookupName :: [Exp] -> Name -> Context t Info
lookupName es n = do
  minfo <- asks (lookup n)
  case minfo of
    Nothing -> throwError (UnboundE n es)
    Just info -> return info

lookupVar :: [Exp] -> Name -> Context t (Scheme Ty)
lookupVar es n = do
  info <- lookupName es n
  case info of
    HasType ty -> return ty
    _          -> throwError (UnboundE n es)

lookupTyLit :: Name -> Context t Kind
lookupTyLit n = do
  info <- lookupName [] n
  case info of
    HasKind k  -> return k
    _          -> throwError (UnboundE n [])

lookupTyVar :: Name -> Context t Kind
lookupTyVar = lookupTyLit

tyInfLit :: Literal -> Context Ty Ty
tyInfLit (LitInt _)  = return (LitT "Int")
tyInfLit (LitChar _) = return (LitT "Char")
tyInfLit (LitCon n)  = lookupVar [] n >>= freshInst

tyProjPat :: [Exp] -> Ty -> Pat -> Context Ty (Subst Ty)
tyProjPat _  _  WildP      = return []
tyProjPat _  t (VarP n)    = return [(n, t)]
tyProjPat es t (ConP n names) = do
  conTy <- lookupVar es n >>= freshInst
  fiTys <- freshVar
  withU $ do
    unify conTy (fiTys .-> t)
    ts <- unfoldArr <$> reify fiTys

--    when (length ts /= length names) $ do
--         throwError (ConArityMismatchE es)

    return (zip names ts)
  where
    unfoldArr :: Ty -> [Ty]
    unfoldArr = undefined

tyProjPat _ _ (LitP _) = error "tyProjPat"



tyInfW :: Exp -> Context Ty Ty
tyInfW = tyInf []
    where
      tyInf es e = go (e : es) e

      go _   Bot    = freshVar

      -- TODO: keep zipper of Exp in context env
      go _  (Lit l) = tyInfLit l
      go es (Var n) = lookupVar es n >>= freshInst
      go es (Abs n e) = do
        t  <- freshVar
        t2 <- bindLocal n (Mono t) (tyInf es e)
        t1 <- withU $ do reify t
        return (t1 .-> t2)

      go es (App e1 e2) = do
        t1 <- tyInf es e1
        t2 <- substLocal (tyInf es e2)
        f  <- freshVar
        withU $ do
          t1' <- reify t1
          unify t1' (t2 .-> f)
          reify f

      go es (Let n e1 e2) = do
        t1 <- tyInf es e1
        substLocal $ do
          s1 <- generalizeM t1
          bindLocal n s1 $ tyInf es e2

      go es (Case e1 alts) = do freshVar
{-        t1 <- tyInf es e1
        ts <- forM alts $ uncurry (tyInfAlt t1)
        tyAltsAgree ts
-}
       where
         tyInfAlt :: Ty -> Pat -> Exp -> Context Ty Ty
         tyInfAlt t p e = do
           binds <- undefined --tyInstPat es t p >>= mapM generalizeM
           bindLocalMany binds (tyInf es e)

         tyAltsAgree :: [Ty] -> Context Ty Ty
         tyAltsAgree [] = stringError $ "no one alt " ++ show es
         tyAltsAgree (t : ts) = withU $ do
             undefined -- foldM unify t ts
             reify t

      go es (Ann e tann) = do
        t <- tyInf es e
        mspec <- withU (tann ||= t)
        unless mspec $ do
          throwError (TyMismatchE tann t es)
        return tann


withDef :: Context t a -> Context t a
withDef = bindLocal border err
    where
      err = error "don't use ty of border"


type TyVarEnv = Subst Kind

kdInf :: Ty -> Context Kind Kind
kdInf (LitT n) = lookupTyLit n
kdInf (VarT n) = lookupTyVar n
kdInf (AppT t1 t2) = do
  k1 <- kdInf t1
  k2 <- kdInf t2
  k <- freshVar
  withUKd $ do
    unifyKd k1 (k2 `ArrK` k)
    reify k

kdInf (AbsT _ _  ) = do error "kdInf"
--  k1 <- freshKdVar
--  k2 <- bindLocal n (Mono k1) (kdInf t)

kdChk :: Ty -> Kind -> Context Kind ()
kdChk t ke = do
  ka <- kdInf t
  spec <- withUKd $ do
    unifyKd ke ka
    ke' <- reify ke
    return (ka == ke')

  unless spec $ do
    throwError (KindMismatchE ka ke t)
  return ()

normalizeScheme :: Scheme Ty -> Context t (Scheme Ty)
normalizeScheme = freshInst >=> generalizeM

isSaturatedTy :: Scheme Ty -> Context Kind ()
isSaturatedTy sc = bindKinds sc (`kdChk` Star)
