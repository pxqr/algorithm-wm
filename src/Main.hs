{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Version (showVersion)
import Options.Applicative
import System.Environment
import Paths_algorithm_wm (version)

import REPL

data Args = Repl Settings
          | ShowVersion

settingsP :: Parser Settings
settingsP = Settings
  <$> strOption
      (  long    "prompt"
      <> metavar "STR"
      <> value   "*>" <> showDefault
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

argsP :: Parser Args
argsP = Repl <$> settingsP
    <|> flag' ShowVersion versionP
  where
   versionP =
        long  "version"
     <> short 'V'
     <> short '?'
     <> help  "Show version and exit."

opts :: ParserInfo Args
opts = info (helper <*> argsP)
  (  fullDesc
  <> progDesc ""
  <> header   ""
  )

run :: Args -> IO ()
run ShowVersion = putStrLn (showVersion version)
run (Repl s)    = runRepl s

main :: IO ()
main = execParser opts >>= run
