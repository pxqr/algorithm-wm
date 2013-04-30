{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module REPL
       ( REPL, Settings(..)
       , runRepl
       , cmdP
       , humanInput, putHumanLike
       ) where

import Control.Applicative ((<$>), some)
import Control.Arrow (second)
import Control.Monad.State
import Control.Exception as E
import Control.Concurrent
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List as L
import Data.Ord
import Data.Typeable (Typeable)
import Text.Parsec as P
import Text.Parsec.String
import Text.PrettyPrint.ANSI.Leijen ((<+>), indent, pretty, line, Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import System.INotify
import qualified System.Console.Haskeline as Line
import System.Random
import System.FilePath ((</>))

import AST
import Module
import Program
import Parser
import TC
import TyError
import Unify

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
  \  \":load some/path\" and \":l some/path\" are the same\n\
  \\n\
  \Many commands support context and type aware completions;\n\
  \just press <TAB> every time you type and see that will happed.\n"



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

instance PP.Pretty Cmd where
  pretty Quit = "quit"
  pretty (Help cmd) = "help" <+> pretty cmd
  pretty (Eval e)   = "eval" <+> pretty e
  pretty (TypeOf e) = "type of" <+> pretty e
  pretty (KindOf t) = "kind of" <+> pretty t
  pretty cmd        = PP.text (show cmd)

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
      _ <- char ':'
      let long  = P.try (string name) >> return ()
      let short = P.char (head name) >> return ()
      long <|> short

    withArgsP name = arglessP name >> some space >> return ()

data Settings = Settings {
    sePrompt     :: String
  , seStdlib     :: String
  , seShowTyEnv  :: Bool
  , seShowParsed :: Bool
  , seHistoryPath :: FilePath
  } deriving Show

data RState = RState {
    stSettings :: Settings

  , stINotify  :: INotify
  , stWD       :: Maybe WatchDescriptor

  , stProgram  :: Program
  , stCurrent  :: Maybe FilePath
  }

type RMonad = StateT RState IO
type REPL = Line.InputT RMonad

data REPLException = TypecheckFail TyError
                   | ParserFail P.ParseError
                   deriving (Show, Typeable)

instance Exception REPLException

instance PP.Pretty REPLException where
  pretty (ParserFail e) = PP.vsep (map PP.text (lines (show e)))
  pretty (TypecheckFail e) = pretty e

ppTyEnv :: TyEnv -> Doc
ppTyEnv = PP.vsep . map pretty . reverse . mapMaybe mkBind
    where
      mkBind (n, HasType ty) = Just (SigD n ty)
      mkBind (n, HasKind kd) = Just (DataD n kd [])

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
      Left err -> throw (ParserFail err)

printParsed :: Program -> REPL ()
printParsed p = do
  se <- lift $ gets stSettings
  when (seShowParsed se) $ liftIO $
    print $ "Parsed module:" <> line <> indent 4 (pretty p)

typecheck :: Program -> REPL TyEnv
typecheck p = liftIO $ do
  case checkProgram p of
    Right tyEn -> return tyEn
    Left  err  -> throw (TypecheckFail err)

printTyEnv :: TyEnv -> REPL ()
printTyEnv tyEn = do
  se <- lift $ gets stSettings
  when (seShowTyEnv se) $ liftIO $
    print $ "Type environment:" <> line <> indent 4 (ppTyEnv tyEn)

untrackCurrent :: REPL ()
untrackCurrent = do
    lift (gets stWD) >>= liftIO . untrackWD
    lift $ modify (\st -> st { stWD = Nothing })
  where
    untrackWD Nothing   = return ()
    untrackWD (Just wd) = removeWatch wd

trackFile :: FilePath -> REPL ()
trackFile path = do
    ino <- lift (gets stINotify)
    st  <- lift get
    _   <- liftIO $ addWatch ino [Modify] path (handler st)
    return ()
  where
    handler _ Modified { isDirectory = False, maybeFilePath = Nothing  } = do
--      putStrLn "Note that file change so might like to reload it..."
      return ()


    handler _ e = putStrLn $ "warning: skipping event " ++ show e

type HumanInput = [(Char, Int)]

humanInput :: String -> IO HumanInput
humanInput str = forM str $ \c -> do
  let msec = 1000
  del <- randomRIO (10 * msec, 100 * msec)
  return (c, del)

putHumanLike :: String -> REPL ()
putHumanLike str = do
  hi <- liftIO $ humanInput str
  forM_ hi $ \(c, d) -> do
    liftIO $ threadDelay d
    Line.outputStr [c]

quit :: REPL ()
quit = untrackCurrent

help :: Cmd -> REPL ()
help (Help ~(Help _)) = do
  putHumanLike "Ok, I help you this time...\n"
  liftIO $ putStrLn cmdDesc

help c = liftIO $ print c

load :: FilePath -> REPL ()
load path = do
  p  <- parseAll path
  printParsed p
  te <- typecheck p
  printTyEnv te
  lift $ modify (\st-> st { stProgram = p, stCurrent = Just path })

  liftIO $ print (PP.green "Ok, loaded modules:" <+>
                  PP.hsep (PP.punctuate PP.comma (map pretty (moduleNames p))))

  untrackCurrent
  trackFile path

reload :: REPL ()
reload = do
  mpath <- lift $ gets stCurrent
  case mpath of
    Nothing -> liftIO $ putStrLn "No one module loaded."
    Just path -> load path

browse :: REPL ()
browse = do
  p <- lift  $ gets stProgram
  if isEmptyProgram p then
      liftIO $ putStrLn "There is nothing to show. Load module with :load."
    else do
      liftIO $ print (pretty p)
      env <- typecheck p
      liftIO $ print (ppTyEnv env)

eval :: Exp -> REPL ()
eval e = do
  let interactiveName = "it"
  prg <- lift $ gets (flip addDec (FunD interactiveName [] e) . stProgram)
  env <- typecheck prg
  let sc  = either fatalError id (inferTy e env)
  let val = fromMaybe fatalError (execName interactiveName prg)
  liftIO $ print $ if isFunc (instQs sc)
    then pretty (PP.red "unable to show function:")
            <+> pretty (renameScheme sc)
    else pretty val <+> PP.colon <+> pretty (renameScheme sc)

 where
   fatalError :: forall a . a
   fatalError = error "REPL.eval: impossible happen"

typeOf :: Exp -> REPL ()
typeOf e = do
  p   <- lift $ gets stProgram
  env <- typecheck p
  liftIO $ case inferTy e env of
    Left er -> print (pretty er)
    Right sc -> print (pretty e <+> PP.colon
                        <+> pretty (renameScheme sc))

kindOf :: Ty -> REPL ()
kindOf t = do
  p   <- lift $ gets stProgram
  env <- typecheck p
  liftIO $ case inferKd t env of
    Left e -> print (pretty e)
    Right k -> print (pretty t <+> PP.colon <+> pretty k)

infoOf :: Name -> REPL ()
infoOf n = do
  p <- lift $ gets stProgram
  let info = lookupNamePrg n p
  liftIO $ if null info
    then putStrLn $ "There is no " ++ n
    else mapM_ (print . pretty) info

suppressE :: Cmd -> REPL () -> REPL ()
suppressE cmd action = Line.catch action handler
  where
    handler :: REPLException -> REPL ()
    handler e = liftIO $ print $
       PP.red (PP.text "Error occurred:") <> PP.line <>
         PP.indent 2 (pretty e) <> PP.line <>
       PP.red (PP.text "While executing:") <> PP.line <>
         PP.indent 2 (pretty cmd)



execCmd :: Cmd -> REPL ()
execCmd    Quit       = quit
execCmd   (Help c)    = help c >> loop
execCmd   (Set  _)    = liftIO (putStrLn "Not implemented") >> loop
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
  pstr <- lift (gets (sePrompt . stSettings))
  mstr <- Line.getInputLine $ show $ PP.blue $ PP.text pstr
  case mstr of
    Just str | not (all isSpace str) -> return str
    _ -> prompt

loop :: REPL ()
loop = do
  str <- prompt
  case parse cmdP ":interactive:" str of
    Left  e   -> liftIO (print e) >> loop
    Right cmd -> execCmd cmd

completer :: Line.CompletionFunc RMonad
completer inp@(rpref, suff) = do
    case dropWhile isSpace (reverse rpref) of
      ":" -> return (rpref, cmdCompletions)
      s | ":l " `isPrefixOf` s -> Line.completeFilename inp
        -- TODO for each command
        | ":l"  `isPrefixOf` s -> Line.completeWord Nothing ""
                                     (const $ return $ [Line.simpleCompletion ":load "]) inp
      s | ":k " `isPrefixOf` s -> do
         bs <- nub . sortBy (comparing fst) <$> gets (dataBindsPrg . stProgram)
         let spaceSyms = "()<>- "
         let env = map (second HasKind) bs
         let varNm = "it"
         let tyStr = drop 2 s ++ " " ++ varNm ++ " " ++ suff
         let colorBind c n kd = (n, c (PP.text n) <+> PP.colon <+> pretty kd)

         case parseTy tyStr of
           Right ty
            | Right mostCmnKdSc <- inferKdOf varNm ty env -> do
             let mostCmnKd = instQs mostCmnKdSc
             let (ok, nok) = partition (isUnifiable mostCmnKd . snd) bs

             let okAlts  = map (uncurry (colorBind (PP.underline . PP.green))) ok
             let nokAlts = map (uncurry (colorBind PP.red)) nok
             let note = ("", PP.green "<----- kind aware completions ----->")
             let alts = [note] ++ nokAlts ++ okAlts
             Line.completeWord Nothing spaceSyms (genericCompl alts) inp

           _ -> do
             let note = ("", PP.red "<----- all available completions ----->")
             let tyAlts = map (uncurry (colorBind id)) bs
             let alts   = note : tyAlts
             Line.completeWord Nothing spaceSyms (genericCompl alts) inp

      s | (":i " `isPrefixOf` s) || s == "" -> do
         ns <- gets (sort . nub . decNamesPrg . stProgram)
         Line.completeWord Nothing " " (regularCompl ns) inp

      _ -> Line.noCompletion inp

  where
   cmdCompletions = map mkCompl cmds
     where
       cmds      = [ "quit", "help", "set", "load"
                   , "reload", "browse", "type", "kind", "info"
                   ]
       mkCompl c = Line.Completion [head c] c True

   regularCompl ns = genericCompl (zip ns (map (PP.blue . PP.text) ns))

   genericCompl ns str = return $ map mkCompl $
      filter ((str `isPrefixOf`) . fst) ns
         where
           ppAlter al@(x : _)
             | isUpper x = show (PP.blue (PP.text al))
             | otherwise = al
           ppAlter _ = []

           mkCompl (n, ann) = Line.Completion n (show ann) True

inIO :: RState -> REPL () -> IO ()
inIO s r = evalStateT (Line.runInputT lineSettings r) s
  where
    lineSettings = Line.Settings completer histPath True
    histPath = Just $ seHistoryPath $ stSettings s

runRepl :: Settings -> IO ()
runRepl s = do
  print $ "Hi! Type" <+> PP.blue ":h"    <+> "or"
                     <+> PP.blue ":help" <+>"for help."

  withINotify $ \ino -> do
    let initState = RState s ino Nothing emptyProgram Nothing
    inIO initState $ do
      load (seStdlib s </> "Prelude.hs")
      loop
    return ()

  putStrLn "Bye, bye...                         ._."
