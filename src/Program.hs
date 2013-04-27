module Program
       ( Program
       , parseProgram, checkProgram, execProgram
       ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)

import Module
import Parser
import TC
import Eval


newtype Program = Program { getProgram :: [Module] }

instance Pretty Program where
  pretty = vsep . punctuate linebreak . map pretty . getProgram

parseProgram :: FilePath -> IO (Either String Program)
parseProgram path = do
  mm <- parseFile path
  case mm of
    Left e -> return (Left (show e))
    Right m -> return (Right (Program [m]))

checkProgram :: Program -> Result TyEnv
checkProgram = checkModule . head . getProgram

execProgram :: Program -> Maybe Value
execProgram = evalMain . head . getProgram