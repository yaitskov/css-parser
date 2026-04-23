module CssParser.At.Namespace where

import CssParser.Ident (Ident)
import CssParser.Prelude
import CssParser.Rule.Value ( Source )
import CssParser.Show ( CssShow(..) )

data Namespace = Namespace (Maybe Ident) Source
  deriving (Show, Eq, Generic)

instance CssShow Namespace where
  toCssText (Namespace i s) =
    "@namespace " <> maybe "" ((<> " ") . toCssText) i <> toCssText s <> ";"
