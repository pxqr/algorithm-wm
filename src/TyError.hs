{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module TyError where

import Control.Monad.Error
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import AST
import Unify

data ExpPath = LApp Exp
             | RApp Exp
             | NAbs Name
               deriving Show

type ExpZipper = ([ExpPath], Exp)

moveBack :: ExpPath -> Exp -> Exp
moveBack (LApp l) e = App l e
moveBack (RApp r) e = App e r
moveBack (NAbs  n) e = Abs n e

moveTop :: Exp -> [ExpPath]-> Exp
moveTop = foldr moveBack

newtype ExpTrace = ExpTrace ([ExpPath], Exp)
                   deriving Show

data TyError = UnificationE (UFailure Ty)
             | KdUnificationE (UFailure Kind)
             | UnboundE     Name    [Exp]
             | TyMismatchE  Ty Ty   [Exp]
             | KindMismatchE Kind Kind Ty
             | RedefineE    Name
             | ConArityMismatchE [Exp]
             | StrMsg String

instance Error TyError where
    noMsg  = StrMsg "<unknown error>"
    strMsg = StrMsg

ppExpTrace :: [Exp] -> Doc
ppExpTrace = vcat . map (\e -> red "in expression: " <+> pretty e)

instance Pretty TyError where
    pretty (UnificationE e) = pretty e

    pretty (UnboundE     n cxt)     =
        red "not bound:" <+> pretty n </> ppExpTrace cxt

    pretty (TyMismatchE t1 t2 cxt) =
        red "Couldn't match expected type:" <+>
           align (pretty t1 <+> red "with actual type:" </>
                  pretty t2) </> ppExpTrace cxt

    pretty (KindMismatchE k1 k2 t) =
        red "Couldn't match expected kind:" <+> pretty k1 </>
                        "with actual kind:" <+> pretty k2 </>
                                 "in type:" <+> pretty t

    pretty (StrMsg s)           = red (text s)

stringError :: MonadError TyError m => String -> m a
stringError = throwError . StrMsg
