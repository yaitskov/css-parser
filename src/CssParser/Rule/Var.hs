module CssParser.Rule.Var where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Show ( CssShow(..) )

newtype Var = Var Ident deriving (Show, Eq, Ord, Generic)

instance CssShow Var where
  toCssText (Var i) = "--" <> toCssText i
