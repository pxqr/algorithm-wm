{-# LANGUAGE OverloadedStrings #-}
module REPL
       ( REPL, Settings(..)
       , runRepl
       ) where

import Control.Applicative ((<$>), some)
import Control.Monad.State
import Control.Exception as E
import Data.Monoid
import Data.Maybe
import Text.Parsec as P
import Text.Parsec.String
import Text.PrettyPrint.ANSI.Leijen ((</>), indent, pretty, line, Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import System.INotify
import System.Console.Readline
import System.Console.ANSI


import TC
import Module
import Program


data Cmd = Quit
         | Load FilePath
         | Eval
         | TypeOf
         | KindOf
         | InfoOf
           deriving Show

cmdP :: Parser Cmd
cmdP = many space >> choice (map P.try
  [ string ":q"  >> return Quit
  , string ":l " >> (Load <$> some anyChar)
  , string ":t " >> return TypeOf
  , string ":k " >> return KindOf
  , string ":i " >> return InfoOf
  ])

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

load :: FilePath -> REPL ()
load path = liftIO $ do
  mm <- parseProgram path
  case mm of
    Left s -> print $ PP.hang 4 ((PP.red "Unable to parse:") </> PP.text (show s))
    Right m -> do
      print $ "Parsed module:" <> line <>
                indent 4 (pretty m)
      case checkProgram m of
        Left err -> print (pretty err)
        Right tyEn -> do
            print $ "Type environment:" <> line <>
                        indent 4 (ppTyEnv tyEn)
            case execProgram m of
              Nothing -> putStrLn "There is no main. Nothing to eval."
              Just va -> print $ "Output:" </> indent 4 (pretty va)

eval :: REPL ()
eval = liftIO $ print "eval"

typeOf :: REPL ()
typeOf = liftIO $ print "type of"

kindOf :: REPL ()
kindOf = liftIO $ print "kind of"


execCmd :: Cmd -> REPL ()
execCmd  Quit       = quit
execCmd (Load path) = load path >> loop
execCmd (Eval     ) = eval >> loop
execCmd (TypeOf   ) = typeOf >> loop
execCmd (KindOf   ) = kindOf >> loop

prompt :: REPL String
prompt = do
  mstr <- liftIO $ readline "*> "
  case mstr of
    Nothing -> prompt
    Just str -> return str

loop :: REPL ()
loop = do
  str <- prompt
  case parse cmdP ":interactive:" str of
    Left  e   -> liftIO (print e) >> loop
    Right cmd -> do
--      handle (handler cmd) $ do
        execCmd cmd
        liftIO (addHistory str)
 where
   handler :: Cmd -> SomeException -> IO ()
   handler cmd e = putStrLn ("Error occurred:\n"
                       ++ show e ++ "\n"
                       ++ "While executing:\n"
                       ++ show cmd
                        )

runRepl :: Settings -> IO ()
runRepl s =
  withINotify $ \ino -> do
    execStateT loop (RState s ino Nothing)
    return ()
