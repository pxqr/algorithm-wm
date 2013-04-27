{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Data.Monoid
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