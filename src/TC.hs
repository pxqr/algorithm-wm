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


runContext :: Num s => Monad m => ReaderT r (StateT s m) a -> r -> m a
runContext c env = evalStateT (runReaderT c env) 0

initTyEnv :: TyEnv
initTyEnv = [ ("->",        HasKind $ ArrK Star (ArrK Star Star))
            ]

runTI :: TyEnv -> Context t a -> Result a
runTI env c = runUnifier (runContext c (initTyEnv ++ env))


inferTy :: Exp -> TyEnv -> Result (Scheme Ty)
inferTy e env = runTI env $ tyInfW e >>= generalizeM

inferDecTy :: Name -> Exp -> TyEnv -> Result (Scheme Ty)
inferDecTy n e env = runTI env $ withDef $
  recDef n (tyInfW e) >>= withU . reify  >>= generalizeM

checkDecTy :: Name -> Exp -> Scheme Ty
              -> TyEnv -> Result (Scheme Ty)
checkDecTy n e sc env = runTI env $ withDef $ do
  expectedTy <- freshInst sc
  recDef n (tyInfW (Ann e expectedTy)) >>= generalizeM

inferKd :: Ty -> TyEnv -> Result (Scheme Kind)
inferKd t env =
  runTI env $ generalizeM t >>= \sc -> do
    bindKinds sc (kdInf >=> generalizeM)

inferKdOf :: Name -> Ty -> TyEnv -> Result (Scheme Kind)
inferKdOf n t env = do
  runTI env $ generalizeM t >>= \sc -> do
    bindKinds sc $ \ty -> do
      kdInf ty
      lookupTyVar n >>= withUKd . reify >>= generalizeM


data Info = HasType (Scheme Ty)
          | HasKind Kind
            deriving Show

type TyEnv = [(Name, Info)]

type Result = Either TyError

type Index = Int
type Context t = ReaderT TyEnv (StateT Index (UnifierM t Result))

generalizeM :: Term a a => a -> Context t (Scheme a)
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

withDef :: Context t a -> Context t a
withDef = bindLocal border err
  where
    err = error "don't use ty of border"

bindLocalMany :: Subst (Scheme Ty) -> Context t a -> Context t a
bindLocalMany bs = local (map (second HasType) bs ++)

bindLocalKd :: Name -> Kind -> Context t a -> Context t a
bindLocalKd n k = local ((n, HasKind k) :)

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

applyTy :: Ty -> Ty -> Context Ty Ty
applyTy t1 t2 = do
  f  <- freshVar
  withU $ do
    t1' <- reify t1
    unify (t2 .-> f) t1'
    reify f

recDef :: Name -> Context Ty Ty -> Context Ty Ty
recDef n c = do
  t <- freshVar
  t' <- bindLocal n (Mono t) c -- TODO
  withU $ unify t t' >> reify t'


unifyMany :: [Ty] -> Context Ty Ty
unifyMany [] = freshVar
unifyMany (t : ts) = foldM (\a x -> withU (unify a x >> reify a)) t ts

tyInfPat :: Pat -> Context Ty Ty
tyInfPat  WildP      = freshVar
tyInfPat (VarP _)    = freshVar
tyInfPat (ConP n ns) = do
  ct  <- tyInfW (ConE n)
  fty <- mapM (const freshVar) ns
  foldM applyTy ct fty

tyInfPats :: [Pat] -> Context Ty Ty
tyInfPats ps = mapM tyInfPat ps >>= unifyMany

tyProjPat :: [Exp] -> Ty -> Pat -> Context Ty (Subst (Scheme Ty))
tyProjPat _  _    WildP      = return []
tyProjPat _  t (VarP n)    = return [(n, Mono t)]
tyProjPat _  _ (ConP _ []) = return []
tyProjPat es t (ConP n [fin]) = do
  conTy <- lookupVar es n >>= freshInst

  fr    <- freshVar
  patTy <- applyTy conTy fr

  fr' <- withU $ do
    unify patTy t
    reify fr

  return [(fin, Mono fr')]

tyProjPat es t (ConP n [a, b]) = do
  conTy <- lookupVar es n >>= freshInst

  fra    <- freshVar
  frb    <- freshVar
  patTy  <- applyTy conTy fra >>= (`applyTy` frb)

  withU $ do
    unify patTy t

  return [(a, Mono fra), (b, Mono frb)]

tyProjPat _ _ _ = error "tyProjPat: not implemented yet"


tyInfW :: Exp -> Context Ty Ty
tyInfW = tyInf []
    where
      tyInf es e = go (e : es) e

      go _   Bot    = freshVar

      -- TODO: keep zipper of Exp in context env
      go es (Var n) = lookupVar es n >>= freshInst
      go es (ConE n) = lookupVar es n >>= freshInst
      go es (Abs n e) = do
        t  <- freshVar
        t2 <- bindLocal n (Mono t) (tyInf es e)
        t1 <- withU $ do reify t
        return (t1 .-> t2)

      go es (App e1 e2) = do
        t1 <- tyInf es e1
        t2 <- substLocal (tyInf es e2)
        applyTy t1 t2

      go es (Let n e1 e2) = do
        rt <- freshVar
        bindLocal n rt $ do
          t1 <- tyInf es e1
          substLocal $ do
            s1 <- generalizeM t1
            bindLocal n s1 $ tyInf es e2

      go es (Case e1 alts) = do
        t1  <- tyInf es e1
        tas <- tyInfPats (map fst alts)
        t1' <- withU $ unify t1 tas >> reify t1

        ts <- forM alts $ uncurry (tyInfAlt t1')
        tyAltsAgree ts

       where
         tyInfAlt :: Ty -> Pat -> Exp -> Context Ty Ty
         tyInfAlt t p e = do
           binds <- tyProjPat es t p
           bindLocalMany binds (tyInf es e)

         tyAltsAgree :: [Ty] -> Context Ty Ty
         tyAltsAgree = unifyMany

      go es (Ann e tann) = do
        t <- tyInf es e
        mspec <- withU (t ||= tann)
        unless mspec $ do
          throwError (TyMismatchE tann t es)
        return tann


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

kdInf (AbsT n t) = do
  k  <- freshVar
  k2 <- bindLocalKd n k (kdInf t)
  k1 <- withUKd $ reify k
  return (k1 `ArrK` k2)

kdChk :: Ty -> Kind -> Context Kind ()
kdChk t ke = do
  ka <- kdInf t
  spec <- withUKd $ do
    unifyKd ke ka
    ka' <- reify ka
    ke' <- reify ke
    return (ka' == ke')

  unless spec $ do
    throwError (KindMismatchE ke ka t)
  return ()

normalizeScheme :: Scheme Ty -> Context t (Scheme Ty)
normalizeScheme = freshInst >=> generalizeM

isSaturatedTy :: Scheme Ty -> Context Kind ()
isSaturatedTy sc = bindKinds sc (`kdChk` Star)
