module CssParser.At where

import CssParser.Prelude
    ( Eq, Ord, Show, IsString, Generic, Semigroup((<>)), Text )
import CssParser.Show ( CssShow(..), encodeStringLiteral )

newtype Charset = Charset Text deriving newtype (Show, Eq, Ord, IsString) deriving (Generic)

instance CssShow Charset where
  toCssText (Charset cs) = "@charset " <> encodeStringLiteral cs <> ";"
