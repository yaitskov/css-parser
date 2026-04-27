module CssParser.At.Supports where

import CssParser.At.MediaQuery
import CssParser.Norm
import CssParser.Prelude
import CssParser.Show

data FeatureQuery
  = FqMediaFeature MediaFeature
  | FqParen FeatureQuery
  | FqBop AndOr FeatureQuery FeatureQuery
  | FqNot FeatureQuery
  deriving (Show, Eq, Ord, Generic)

instance CssShow FeatureQuery where
  toCssText x = case x of
    FqMediaFeature mf -> "(" <> toCssText mf <> ")"
    FqParen fq -> "(" <> toCssText fq <> ")"
    FqBop bop fql@FqBop{} fqr ->
      "(" <> toCssText fql <> ")" <> toCssText bop <> toCssText fqr
    FqBop bop fql fqr ->
      toCssText fql <> toCssText bop <> toCssText fqr
    FqNot fq@FqNot {} -> "not (" <> toCssText fq <> ")"
    FqNot fq@FqBop {} -> "not (" <> toCssText fq <> ")"
    FqNot fq -> "not " <> toCssText fq

instance Norm FeatureQuery where
  normalize = \case
    mf@FqMediaFeature {} -> mf
    FqParen (mf@FqMediaFeature {}) -> mf
    FqParen (p@FqParen {}) -> normalize p
    FqParen p -> FqParen $ normalize p
    FqBop bop (FqParen l) r -> FqBop bop (normalize l) (normalize r)
    FqBop bop l r -> FqBop bop (normalize l) (normalize r)
    FqNot (FqNot fq) -> normalize fq
    FqNot (FqParen fq) -> FqNot $ normalize fq
    FqNot o -> FqNot $ normalize o
