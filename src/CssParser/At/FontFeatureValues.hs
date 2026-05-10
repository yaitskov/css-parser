module CssParser.At.FontFeatureValues where

import CssParser.Ident
import CssParser.Rule.Value
import CssParser.At.Keyframe
import CssParser.Prelude
import CssParser.Show

newtype IdentList = IdentList (NonEmpty Ident) deriving newtype (Eq, Ord, Show) deriving (Generic)
instance CssShow IdentList where
  toCssText (IdentList l) = unwords (toCssText <$> toList l)

data FontFeatureEntry = FontFeatureEntry Ident (SslNe Unsigned)
  deriving (Eq, Ord, Show, Generic)
instance CssShow FontFeatureEntry where
  toCssText (FontFeatureEntry i l)
    = toCssText i <> ": " <> toCssText l
instance ShowSpaceBetween FontFeatureEntry FontFeatureEntry where
  cssSpace _ _ = ";"
data FontFeatureValuesSubBlock
  = FontFeatureValuesSubBlock BrowserPrefix Ident [FontFeatureEntry]
  deriving (Eq, Ord, Show, Generic)

instance CssShow FontFeatureValuesSubBlock where
  toCssText (FontFeatureValuesSubBlock bp i ps) =
    "@" <> toCssText bp <> toCssText i <> " {" <> toCssText ps <> "}"

instance ShowSpaceBetween FontFeatureValuesSubBlock FontFeatureValuesSubBlock where
  cssSpace _ _ = ""
data FontFeatureValues
  = FontFeatureValues
  { name :: Either LiteralString IdentList
  , props :: [PropEntry]
  , blocks :: [FontFeatureValuesSubBlock]
  }
  deriving (Show, Eq, Ord, Generic)

instance CssShow FontFeatureValues where
  toCssText ff =
    "font-feature-values " <> toCssText ff.name <> " {" <>
    toCssText ff.props  <> toCssText ff.blocks <> "}"
