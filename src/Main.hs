{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Data.Monoid
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char
import Data.Maybe
import Data.List as L
import qualified Data.Text.IO as T
import Data.Text (Text, unpack)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)

import System.INotify
import System.Environment
import System.Directory
import System.Console.ANSI
import Debug.Trace (trace)

import AST
import Unify
import Eval
import Module
import Parser
import TC
import TyError


traceEnvUp :: Context t TyEnv
traceEnvUp = takeWhile ((border /=) . fst) <$> tyEnv


prettySubst :: Pretty t => Subst t -> Doc
prettySubst = vcat . map (\(n, t) -> text n <+> text "|->" <+> align (pretty t))


ppTyEnv :: TyEnv -> Doc
ppTyEnv = pretty . reverse . map (uncurry SigD) . mapMaybe tyBind
    where
      tyBind (n, HasType ty) = Just (n, ty)
      tyBind _               = Nothing

----------------------------------------------------------------------------------

--runUnifier :: Unifier Ty a -> Result a
runUnifier u = evalStateT u []

--runContext :: Context a -> TyEnv -> Unifier Ty a
runContext c env = evalStateT (runReaderT c env) 0

runTI :: TyEnv -> Context t a -> Result a
runTI env c = runUnifier (runContext c env)

inferTy :: Exp -> TyEnv -> Result (Scheme Ty)
inferTy e env = runTI env (withDef (tyInfW e >>= generalizeM))

initTyEnv :: TyEnv
initTyEnv = [ ("->",        HasKind $ ArrK Star (ArrK Star Star))
            ]


checkModule :: Module -> Result TyEnv
checkModule m = do
    grm <- groupDec (modDecs m)
    checkNames grm
    foldM checkDec initTyEnv grm
  where
    groupDec :: [Dec] -> Result [[Dec]]
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

--------------------------------------------------------------------------------
run :: FilePath -> IO ()
run path = do
  mod <- parseFile path
  case mod of
    Left s -> print $ hang 4 ((red "Unable to parse:") </> text (show s))
    Right m -> do
      print $ "Parsed module:" <> line <>
                indent 4 (pretty m)
      case checkModule m of
        Left err -> print (pretty err)
        Right tyEnv -> do
            print $ "Type environment:" <> line <>
                        indent 4 (ppTyEnv tyEnv)
            case evalMain m of
              Nothing -> putStrLn "There is no main. Nothing to eval."
              Just va -> print $ "Output:" </>
                              indent 4 (pretty va)


main :: IO ()
main = do
  [path] <- getArgs
  run path
  ino <- initINotify
  wd <- addWatch ino [Modify] path $ \e ->
        case e of
          Modified { isDirectory = False, maybeFilePath = Nothing  }
              | otherwise -> clearFromCursorToScreenBeginning >> run path
          _ -> putStrLn $ "warning: skipping event " ++ show e

  getLine
  removeWatch wd
  killINotify ino
  putStrLn "Bye, bye! ._."