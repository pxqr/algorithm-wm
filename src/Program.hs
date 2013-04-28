module Program
       ( Program, emptyProgram
       , lookupNamePrg
       , parseProgram, checkProgram, execProgram
       ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)

import Eval
import Module
import Name
import Parser
import TC


newtype Program = Program { getProgram :: [Module] }

instance Pretty Program where
  pretty = vsep . punctuate linebreak . map pretty . getProgram

emptyProgram :: Program
emptyProgram = Program []

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

lookupNamePrg :: Name -> Program -> [Dec]
lookupNamePrg n = concatMap (lookupNameM n) . getProgram