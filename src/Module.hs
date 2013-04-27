{-# LANGUAGE OverloadedStrings#-}
module Module ( Module(..), Dec(..)
              , decName
              ) where

import Text.PrettyPrint.ANSI.Leijen

import AST
import Name


type ConDef = (Name, Scheme Ty)

data Dec = DataD Name Kind
         | SigD  Name (Scheme Ty)
         | FunD  Name [Name] Exp
           deriving Show

data Module = Module {
    modName :: ModName
  , modDecs :: [Dec]
  }

instance Pretty Dec where
    pretty (DataD n k) = yellow "data" <+> underline (magenta (text n)) <+> colon <+> pretty k
    pretty (SigD  n s) = underline (text n) <+> colon <+> pretty s
    pretty (FunD  n ps e) = underline (text n) <+> hsep (map text ps) <+> equals <+> pretty e

instance Pretty Module where
  pretty m = vsep (modD : map pretty (modDecs m))
    where
      modD = "module" <+> pretty (modName m) <+> "where"

decName :: Dec -> Name
decName (DataD n _) = n
decName (SigD  n _) = n
decName (FunD  n _ _) = n
