{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module TC where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as S
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import AST
import Unify
import TyError

data Info = HasType (Scheme Ty)
          | HasKind Kind
            deriving Show

type TyEnv = [(Name, Info)]

type Result = Either TyError

type Index = Int
type Context = ReaderT TyEnv (StateT Index (UnifierM Ty Result))

generalizeM :: Term a Ty => a -> Context (Scheme a)
generalizeM ty = generalize ty . S.fromList . map fst <$> tyEnv

substTyEnv :: TyEnv -> Subst Ty  -> TyEnv
substTyEnv env s = map substInfoItem env
    where
      substInfoItem (n, HasType ty) = (n, HasType (subst ty s))
      substInfoItem info = info

border :: Name
border = "__expr_border"; -- TODO: remove 

tyEnv :: Context TyEnv
tyEnv = ask

withU :: Unifier Ty a -> Context a
withU = lift . lift . mapStateT (mapLeft UnificationE)
  where
    mapLeft f (Left a)  = Left (f a)
    mapLeft f (Right a) = Right a

fresh :: Context Name
fresh = do 
  i <- get
  modify succ 
  return ('v' : show i)

freshTyVar :: Context Ty
freshTyVar = VarT <$> fresh

freshInst :: Term a Ty => Scheme a -> Context a
freshInst = go []
  where
    go s (Mono t)   = return (subst t s)
    go s (Poly n t) = do 
      f <- freshTyVar
      go ((n, f) : s) t

bindLocal :: Name -> Scheme Ty -> Context a -> Context a
bindLocal n t = local ((n, HasType t) :)

bindLocalMany :: Subst (Scheme Ty) -> Context a -> Context a
bindLocalMany bs = local (map (second HasType) bs ++)

--bindKindLocal :: Name -> Scheme -> Context a -> Context a
--bindKindLocal n t = local ((n, HasType t) :)

substLocal :: Context a -> Context a
substLocal cxt = do 
  s <- withU $ do get
  local (`substTyEnv` s) cxt

lookupName :: [Exp] -> Name -> Context Info
lookupName es n = do 
  minfo <- asks (lookup n) 
  case minfo of 
    Nothing -> throwError (UnboundE n es)
    Just info -> return info

lookupVar :: [Exp] -> Name -> Context (Scheme Ty)
lookupVar es n = do
  info <- lookupName es n
  case info of
    HasType ty -> return ty
    _          -> throwError (UnboundE n es)

lookupTyLit :: Name -> Context Kind
lookupTyLit n = do
  info <- lookupName [] n
  case info of
    HasKind k  -> return k
    _          -> throwError (UnboundE n [])

tyInfLit :: Literal -> Ty
tyInfLit (LitInt i) = LitT "Int"
tyInfLit (LitChar c) = LitT "Char"

tyProjPat :: [Exp] -> Ty -> Pat -> Context (Subst Ty)
tyProjPat _  _  WildP      = return []
tyProjPat _  t (VarP n)    = return [(n, t)]
tyProjPat es t (ConP n names) = do
  conTy <- lookupVar es n >>= freshInst
  fiTys <- freshTyVar
  withU $ do
    unify conTy (fiTys .-> t)
    ts <- unfoldArr <$> reify fiTys

--    when (length ts /= length names) $ do
--         throwError (ConArityMismatchE es)
                    
    return (zip names ts)
             
  where
    unfoldArr :: Ty -> [Ty]
    unfoldArr = undefined


tyInfW :: Exp -> Context Ty
tyInfW = tyInf []
    where
      tyInf es e = go (e : es) e
      
      go _   Bot    = freshTyVar

      -- TODO: keep zipper of Exp in context env
      go _  (Lit l) = return (tyInfLit l)
      go es (Var n) = lookupVar es n >>= freshInst
      go es (Abs n e) = do 
        t  <- freshTyVar
        t2 <- bindLocal n (Mono t) (tyInf es e)
        t1 <- withU $ do reify t
        return (t1 .-> t2)

      go es (App e1 e2) = do 
        t1 <- tyInf es e1
        t2 <- substLocal (tyInf es e2)
        f  <- freshTyVar
        withU $ do 
          t1' <- reify t1 
          unify t1' (t2 .-> f)
          reify f

      go es (Let n e1 e2) = do 
        t1 <- tyInf es e1
        substLocal $ do 
          s1 <- generalizeM t1
          bindLocal n s1 $ tyInf es e2

      go es (Case e1 alts) = do freshTyVar -- TODO: handle polymorphism properly
{-        t1 <- tyInf es e1
        ts <- forM alts $ uncurry (tyInfAlt t1)
        tyAltsAgree ts
-}
       where 
         tyInfAlt :: Ty -> Pat -> Exp -> Context Ty
         tyInfAlt t p e = do
           binds <- undefined --tyInstPat es t p >>= mapM generalizeM 
           bindLocalMany binds (tyInf es e)

         tyAltsAgree :: [Ty] -> Context Ty
         tyAltsAgree [] = stringError $ "no one alt " ++ show es
         tyAltsAgree (t : ts) = withU $ do 
             undefined -- foldM unify t ts
             reify t

      go es e0@(Ann e tann) = do
        t <- tyInf es e
        mspec <- withU (tann ||= t)
        unless mspec $ do
          throwError (TyMismatchE tann t es)
        return tann


withDef :: Context a -> Context a
withDef = bindLocal border err
    where
      err = error "don't use ty of border"


{-
-- TODO: we need proper context and unifier for kind inference
type TyVarEnv = Subst Kind

kindInfer :: Ty -> Context Kind
kindInfer (LitT n) = lookupTyLit n
kindInfer (VarT n) = return Star -- TODO fix that
kindInfer (AppT t1 t2) = do
    k1 <- kindInfer t1
    k2 <- kindInfer t2
    case k1 of
      ArrK k1' k | k1' == k1 -> return k
                 | otherwise -> throwError (KindMismatch k1' k1)
      Star -> throwError (KindMismatch k1' k1)

kindCheck ::  -> Ty -> Kind -> Context ()
kindCheck  = undefined

isSaturatedTy :: Scheme -> Context ()
isSaturatedTy ty = kindCheck ty Star
-}
