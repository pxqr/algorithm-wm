{-# LANGUAGE OverloadedStrings #-}
module REPL
       ( REPL, Settings(..)
       , runRepl
       , cmdP
       ) where

import Control.Applicative ((<$>), some)
import Control.Monad.State
import Control.Monad.Error
import Control.Exception as E
import Data.Monoid
import Data.Maybe
import Data.Char
import Text.Parsec as P
import Text.Parsec.String
import Text.PrettyPrint.ANSI.Leijen ((</>), (<+>),
                                     indent, pretty, line, Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import System.INotify
import System.Console.Readline
import System.Console.ANSI

import AST
import Module
import Program
import Parser
import TC

cmdDesc :: String
cmdDesc =
  ":quit       - Quit from the repl.\n\
  \:help thing - Help about the thing.\n\
  \:set var    - Set repl settings variable.\n\
  \:load path  - Load module from the path.\n\
  \:reload     - Reload currently loaded module.\n\
  \:browse     - Browse currenly loaded module.\n\
  \:type expr  - Show type of the expression.\n\
  \:kind type  - Show kind of the type.\n\
  \:info name  - Show info about the name.\n\
  \ expr       - Eval expression.\n\
  \\n\
  \Every command have short alias consisting of first command letter.\n\
  \For exsample:\n\
  \  \":quit\" and \":q\" are the same\n\
  \  \":load some/path\" and \":l some/path\" are the same"




data Cmd = Quit
         | Help Cmd
         | Set    String
         | Load FilePath
         | Reload
         | Browse
         | Eval   Exp
         | TypeOf Exp
         | KindOf Ty
         | InfoOf Name
           deriving Show

cmdP :: Parser Cmd
cmdP = many space >> choice (map P.try
  [ arglessP "quit"  >> return Quit
  , do withArgsP "load"
       (Load  <$> some anyChar)
         <|> return (Help (Load "path/to/module"))
  , withArgsP "type"   >> TypeOf <$> inRepl expP
  , withArgsP "kind"   >> KindOf <$> inRepl tyP
  , withArgsP "info"   >> InfoOf <$> inRepl nameP
  , arglessP  "browse" >> return Browse
  , arglessP  "reload" >> return Reload
  , arglessP  "help"   >> return (Help (Help undefined))
  , arglessP ""     >> return (Help (Help undefined))
  , Eval   <$> inRepl expP
  ])
  where
    arglessP name = do
      char ':'
      let long  = P.try (string name) >> return ()
      let short = P.char (head name) >> return ()
      long <|> short

    withArgsP name = arglessP name >> some space

data Settings = Settings {
    sePrompt     :: String
  , seStdlib     :: String
  , seShowTyEnv  :: Bool
  , seShowParsed :: Bool
  } deriving Show

data RState = RState {
    stSettings :: Settings

  , stINotify  :: INotify
  , stWD       :: Maybe WatchDescriptor

  , stProgram  :: Program
  , stCurrent  :: Maybe FilePath
  }

type REPL = StateT RState IO

ppTyEnv :: TyEnv -> Doc
ppTyEnv = pretty . reverse . map (uncurry SigD) . mapMaybe tyBind
    where
      tyBind (n, HasType ty) = Just (n, ty)
      tyBind _               = Nothing

{-
traceEnvUp :: Context t TyEnv
traceEnvUp = takeWhile ((border /=) . fst) <$> tyEnv


prettySubst :: Pretty t => Subst t -> Doc
prettySubst = vcat . map (\(n, t) -> text n <+> text "|->" <+> align (pretty t))
-}

parseAll :: FilePath -> REPL Program
parseAll path = do
    mm <- liftIO $ parseProgram path
    case mm of
      Right p -> return p
      Left s -> liftIO $ ioError $ userError (show (ppErr s))
  where
    ppErr s = PP.hang 4 ((PP.red "Unable to parse:") </> PP.text (show s))

printParsed :: Program -> REPL ()
printParsed p = do
  se <- gets stSettings
  when (seShowParsed se) $ liftIO $
    print $ "Parsed module:" <> line <> indent 4 (pretty p)

typecheck :: Program -> REPL TyEnv
typecheck p = liftIO $ do
  case checkProgram p of
    Right tyEn -> return tyEn
    Left  err  -> ioError $ userError (show (pretty err))

printTyEnv :: TyEnv -> REPL ()
printTyEnv tyEn = do
  se <- gets stSettings
  when (seShowTyEnv se) $ liftIO $
    print $ "Type environment:" <> line <> indent 4 (ppTyEnv tyEn)

untrackCurrent :: REPL ()
untrackCurrent = gets stWD >>= liftIO . untrackWD
  where
    untrackWD Nothing   = return ()
    untrackWD (Just wd) = removeWatch wd

trackFile :: FilePath -> REPL ()
trackFile path = do
    ino <- gets stINotify

    liftIO $ addWatch ino [Modify] path handler
    return ()
  where
    handler Modified { isDirectory = False, maybeFilePath = Nothing  } = do
      clearFromCursorToScreenBeginning
--      load path
    handler e = putStrLn $ "warning: skipping event " ++ show e


quit :: REPL ()
quit = untrackCurrent

help :: Cmd -> REPL ()
help (Help ~(Help _)) = liftIO $ putStrLn cmdDesc
help c = liftIO $ print c

load :: FilePath -> REPL ()
load path = do
  p  <- parseAll path
  printParsed p
  te <- typecheck p
  printTyEnv te
  modify (\st-> st { stProgram = p, stCurrent = Just path })
  liftIO $ case execProgram p of
    Nothing -> putStrLn "There is no main. Nothing to eval."
    Just va -> print $ "Output:" </> indent 4 (pretty va)

reload :: REPL ()
reload = do
  mpath <- gets stCurrent
  case mpath of
    Nothing -> liftIO $ putStrLn "No one module loaded."
    Just path -> load path

browse :: REPL ()
browse = do
  p <- gets stProgram
  if isEmptyProgram p then
      liftIO $ putStrLn "There is nothing to show. Load module with :load."
    else do
      liftIO $ print (pretty p)
      env <- typecheck p
      liftIO $ print (ppTyEnv env)

eval :: Exp -> REPL ()
eval e = liftIO $ print e

typeOf :: Exp -> REPL ()
typeOf e = do
  p   <- gets stProgram
  env <- typecheck p
  liftIO $ case inferTy e env of
    Left er -> print (pretty er)
    Right t -> print (pretty e <+> PP.colon <+> pretty t)

kindOf :: Ty -> REPL ()
kindOf t = do
  p   <- gets stProgram
  env <- typecheck p
  liftIO $ case inferKd t env of
    Left e -> print (pretty e)
    Right k -> print (pretty t <+> PP.colon <+> pretty k)

infoOf :: Name -> REPL ()
infoOf n = do
  p <- gets stProgram
  let info = lookupNamePrg n p
  liftIO $ if null info
    then putStrLn $ "There is no " ++ n
    else mapM_ (print . pretty) info

suppressE :: Cmd -> REPL () -> REPL ()
suppressE cmd action = catchError action (handler cmd)
  where
    handler cmd e = liftIO $ putStrLn ("Error occurred:\n"
                       ++ show e ++ "\n"
                       ++ "While executing:\n"
                       ++ show cmd
                        )

execCmd :: Cmd -> REPL ()
execCmd c@Quit       = quit
execCmd   (Help c)   = help c >> loop
execCmd c@(Load path) = do
  suppressE c (load (takeWhile (not . isSpace) path))
  loop
execCmd c@(Reload   ) = suppressE c reload >> loop
execCmd c@(Browse   ) = suppressE c browse >> loop
execCmd c@(Eval   e ) = suppressE c (eval e)   >> loop
execCmd c@(TypeOf e ) = suppressE c (typeOf e) >> loop
execCmd c@(KindOf t ) = suppressE c (kindOf t) >> loop
execCmd c@(InfoOf n ) = suppressE c (infoOf n) >> loop

prompt :: REPL String
prompt = do
  mstr <- liftIO $ readline "*> "
  case mstr of
    Just str | not (all isSpace str) -> return str
    _ -> prompt

loop :: REPL ()
loop = do
  str <- prompt
  case parse cmdP ":interactive:" str of
    Left  e   -> liftIO (print e) >> loop
    Right cmd -> do
      execCmd cmd
      liftIO (addHistory str)

runRepl :: Settings -> IO ()
runRepl s = do
  putStrLn "Hi! Type :h or :help for help."

  withINotify $ \ino -> do
    let initState = RState s ino Nothing emptyProgram Nothing
    execStateT loop initState
    return ()

  putStrLn "Bye, bye...                         ._."
