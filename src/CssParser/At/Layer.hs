module CssParser.At.Layer where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Show ( CssShow(..) )


newtype LayerName = LayerName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

newtype LayerStmt = LayerStmt (NonEmpty LayerName)
  deriving newtype (Show, Eq, Ord) deriving (Generic)

instance CssShow LayerStmt where
  toCssText (LayerStmt ls) =
    "@layer " <> intercalate ", " (fmap toCssText . toList $ ls) <> ";"
