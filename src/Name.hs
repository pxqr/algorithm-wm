module Name where

import Text.PrettyPrint.ANSI.Leijen


type Name = String

newtype ModName = ModName { getModName :: Name }

instance Pretty ModName where
  pretty = blue . text . getModName