module CssParser.At.FontFeatureValues where

import CssParser.Ident
import CssParser.Rule.Value
import CssParser.At.Keyframe
import CssParser.Prelude
import CssParser.Show

newtype IdentList = IdentList (NonEmpty Ident) deriving newtype (Eq, Ord, Show) deriving (Generic)
instance CssShow IdentList where
  toCssText (IdentList l) = intercalate " " (toCssText <$> toList l)

data FontFeatureValuesSubBlock
  = FontFeatureValuesSubBlock Ident [PropEntry]
  deriving (Eq, Ord, Show, Generic)

instance CssShow FontFeatureValuesSubBlock where
  toCssText (FontFeatureValuesSubBlock i ps) =
    "@" <> toCssText i <> " {" <> toCssText ps <> "}"

data FontFeatureValues
  = FontFeatureValues
  { name :: Either LiteralString IdentList
  , props :: [PropEntry]
  , blocks :: [FontFeatureValuesSubBlock]
  }
  deriving (Show, Eq, Ord, Generic)

instance CssShow FontFeatureValues where
  toCssText ff =
    "@font-feature-values " <> toCssText ff.name <> " {" <>
    toCssText ff.props  <> toCssText ff.blocks <> "}"
