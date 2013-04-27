{-# LANGUAGE OverloadedStrings#-}
module Module ( Module(..), Dec(..)
              , decName, checkModule
              ) where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S

import AST
import Name
import Unify
import TC
import TyError


type ConDef = (Name, Scheme Ty)

data Dec = DataD Name Kind [ConDef]
         | SigD  Name (Scheme Ty)
         | FunD  Name [Name] Exp
           deriving Show

data Module = Module {
    modName    :: ModName
  , modImports :: [ModName]
  , modDecs    :: [Dec]
  }

instance Pretty Dec where
    pretty (DataD n k []) =
      yellow "data" <+> underline (magenta (text n)) <+> colon <+> pretty k

    pretty (DataD n k cds) =
      yellow "data" <+> underline (magenta (text n)) <+> colon <+> pretty k <+>
            "where" <>linebreak <>
        indent 2 (vcat (map ppCD cds))
      where
        ppCD (nm, ty) = underline (text nm) <+>  colon <+> pretty ty

    pretty (SigD  n s) = underline (text n) <+> colon <+> pretty s
    pretty (FunD  n ps e) =
      underline (text n) <+> hsep (map text ps) <+> equals <+> pretty e

instance Pretty Module where
  pretty m = vsep (modD : (map ppImport (modImports m) ++ map pretty (modDecs m)))
    where
      modD = "module" <+> pretty (modName m) <+> "where"
      ppImport imp = "import" <+> pretty imp

decName :: Dec -> Name
decName (DataD n _ _) = n
decName (SigD  n _)   = n
decName (FunD  n _ _) = n


initTyEnv :: TyEnv
initTyEnv = [ ("->",        HasKind $ ArrK Star (ArrK Star Star))
            ]


--runUnifier :: Unifier Ty a -> Result a
runUnifier u = evalStateT u []

--runContext :: Context a -> TyEnv -> Unifier Ty a
runContext c env = evalStateT (runReaderT c env) 0

runTI :: TyEnv -> Context t a -> Result a
runTI env c = runUnifier (runContext c env)

inferTy :: Exp -> TyEnv -> Result (Scheme Ty)
inferTy e env = runTI env (withDef (tyInfW e >>= generalizeM))

checkModule :: Module -> Result TyEnv
checkModule m = do
    grm <- groupDec (modDecs m)
    checkNames grm
    foldM checkDec initTyEnv grm
  where
    groupDec (x@(DataD _ _ _)  : xs)   = ([x] :) <$> groupDec xs
    groupDec (sig@(SigD sn _) : xs)
      | fun@(FunD fn _ _)  : xs' <- xs
      , fn == sn                     = ([sig, fun] :) <$> groupDec xs'
      |      otherwise               = stringError msg
      where
        msg = "The type signature for " ++ show sn ++ " lacks an accompanying binding"

    groupDec (x@(FunD _ _ _) : xs)     = ([x] :) <$> groupDec xs
    groupDec [] = return []


    checkCon env (n, sc) = do
      runTI env $ do
        s <- normalizeScheme sc
        isSaturatedTy s
        return (n, s)

    checkSigKd env sc = do
      runTI env $ do
        s <- normalizeScheme sc
        isSaturatedTy s

    checkNames :: [[Dec]] -> Result ()
    checkNames = foldM_ checkName S.empty . map (decName . head)
        where
         checkName s n | n `S.member` s = throwError (RedefineE n)
                       |    otherwise   = return (S.insert n s)


    checkDec :: TyEnv -> [Dec] -> Result TyEnv
    checkDec e [DataD n k cons]  = do
        let env = (n, HasKind k) : e
        cons' <- mapM (checkCon env) cons
        return (map (second HasType) cons' ++ env)

    checkDec env [SigD n sc, FunD _ ps e] = do
      checkSigKd env sc
      ty <- runTI env $ do
        tyAnn <- freshInst sc
        scAnn <- generalizeM tyAnn

        tyInfW (Ann (desugar e ps) tyAnn)
        return scAnn

      return ((n, HasType (renameScheme ty)) : env)

    checkDec env [(FunD n ps e)] = do
      ty <- inferTy (desugar e ps) env
      return ((n, HasType (renameScheme ty)) : env)

    desugar = foldr Abs
