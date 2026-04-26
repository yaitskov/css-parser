module CssParser.At.Layer where

import CssParser.Ident ( Ident(..) )
import CssParser.Prelude
    ( Eq, Ord, Show, IsString, Generic, Semigroup((<>)), NonEmpty )
import CssParser.Show ( CssShow(..), ShowSpaceBetween(..) )

newtype LayerName = LayerName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

newtype LayerStmt = LayerStmt (NonEmpty LayerName)
  deriving newtype (Show, Eq, Ord) deriving (Generic)

instance ShowSpaceBetween LayerName LayerName where
  cssSpace _ _ = ", "

instance CssShow LayerStmt where
  toCssText (LayerStmt ls) = "@layer " <> toCssText ls <> ";"
