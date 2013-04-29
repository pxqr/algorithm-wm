module Program
       ( Program
       , emptyProgram, isEmptyProgram, moduleNames
       , decNamesPrg, dataBindsPrg
       , lookupNamePrg, addDec
       , parseProgram, checkProgram, execProgram, execName
       ) where

import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)
import Text.Parsec

import AST
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

isEmptyProgram :: Program -> Bool
isEmptyProgram (Program []) = True
isEmptyProgram _            = False

moduleNames :: Program -> [ModName]
moduleNames p = map modName (getProgram p)

decNamesPrg :: Program -> [Name]
decNamesPrg = concatMap decNamesMod . getProgram

dataBindsPrg :: Program -> [(Name, Kind)]
dataBindsPrg = concatMap dataBindsMod . getProgram

parseProgram :: FilePath -> IO (Either ParseError Program)
parseProgram path = do
  mm <- parseFile path
  case mm of
    Left e -> return (Left e)
    Right m -> return (Right (Program [m]))

checkProgram :: Program -> Result TyEnv
checkProgram p = concat <$> mapM checkModule (getProgram p)

execName :: Name -> Program -> Maybe Value
execName n = evalName n . head . getProgram

execProgram :: Program -> Maybe Value
execProgram = execName "main"

addDec :: Program -> Dec -> Program
addDec (Program []) d = Program [interactiveModule d]
addDec (Program (x : xs)) d = Program (addDecToMod d x : xs)

lookupNamePrg :: Name -> Program -> [Dec]
lookupNamePrg n = concatMap (lookupNameM n) . getProgram