{-# LANGUAGE OverloadedStrings#-}
module Module ( Module(..), Dec(..)
              , interactiveModule, addDecToMod
              , decName, lookupNameM
              , checkModule
              ) where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Error
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)

import AST
import Name
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

decName :: Dec -> [Name]
decName (DataD n _ cs) = n : map fst cs
decName (SigD  n _)    = [n]
decName (FunD  n _ _)  = [n]

lookupNameM :: Name -> Module -> [Dec]
lookupNameM n = mapMaybe getInfo . modDecs
  where
    getInfo d | (n ==) `any` decName d = Just d
              | otherwise              = Nothing


interactiveModule :: Dec -> Module
interactiveModule d = Module (ModName "REPL.Interactive") [] [d]

addDecToMod :: Dec -> Module -> Module
addDecToMod d m = m { modDecs = modDecs m ++ [d] }

initTyEnv :: TyEnv
initTyEnv = [ ("->",        HasKind $ ArrK Star (ArrK Star Star))
            ]

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
        msg = "The type signature for " ++ show sn
              ++ " lacks an accompanying binding"

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
    checkNames = foldM_ checkName S.empty . concatMap (decName . head)
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
      ty <- checkDecTy n (desugar e ps) sc env
      return ((n, HasType (renameScheme ty)) : env)

    checkDec env [(FunD n ps e)] = do
      ty <- inferDecTy n (desugar e ps) env
      return ((n, HasType (renameScheme ty)) : env)

    checkDec _ _ = error "checkDec: impossible happen"

    desugar = foldr Abs
