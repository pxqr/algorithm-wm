{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)

import System.INotify
import System.Environment
import System.Console.ANSI

import AST
import Eval
import Module
import Parser
import TC


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
  mm <- parseFile path
  case mm of
    Left s -> print $ hang 4 ((red "Unable to parse:") </> text (show s))
    Right m -> do
      print $ "Parsed module:" <> line <>
                indent 4 (pretty m)
      case checkModule m of
        Left err -> print (pretty err)
        Right tyEn -> do
            print $ "Type environment:" <> line <>
                        indent 4 (ppTyEnv tyEn)
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

  _ <- getLine
  removeWatch wd
  killINotify ino
  putStrLn "Bye, bye! ._."