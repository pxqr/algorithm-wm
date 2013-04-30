{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.Version (showVersion)
import Options.Applicative
import System.Environment
import System.FilePath
import System.Directory
import Paths_algorithm_wm (version)

import REPL

data Args = Repl Settings
          | ShowVersion

mkSettingsP :: IO (Parser Settings)
mkSettingsP = do
    tmpDir   <- getTemporaryDirectory
    appName  <- getProgName
    let histPath = tmpDir </> appName
    return $ mkParser histPath
  where
    mkParser histPath = Settings
     <$> strOption
      (  long    "prompt"
      <> metavar "STR"
      <> value   "*> " <> showDefault
      <> help    "Freeform prompt string."
      )

     <*> strOption
      (  long    "stdlib"
      <> metavar "DIR"
      <> value   "./lib" <> showDefault
      <> help    "Path to standart library sources."
      )

     <*> switch
      (  long    "show-ty-env"
      <> help    "Show type environment after load."
      )

     <*> switch
      (  long    "debug"
      <> help    "Show each parsed entity."
      )

     <*> strOption
      (  long    "history"
      <> metavar "FILE"
      <> value   histPath <> showDefault
      <> help    "Path to repl history file."
      )

argsP :: Parser Settings -> Parser Args
argsP setP = Repl <$> setP
         <|> flag' ShowVersion versionP
  where
   versionP =
        long  "version"
     <> short 'V'
     <> short '?'
     <> help  "Show version and exit."

optsP ::  Parser Args -> ParserInfo Args
optsP args = info (helper <*> args)
  (  fullDesc
  <> progDesc ""
  <> header   ""
  )

run :: Args -> IO ()
run ShowVersion = putStrLn (showVersion version)
run (Repl s)    = runRepl s

main :: IO ()
main = mkSettingsP >>= execParser . optsP . argsP >>= run
