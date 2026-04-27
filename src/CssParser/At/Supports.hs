module CssParser.At.Supports where

import CssParser.At.MediaQuery ( MediaFeature, AndOr, toPlainMf )
import CssParser.Ident ( Ident )
import CssParser.Norm ( Norm(..) )
import CssParser.Prelude
import CssParser.Rule.Value
import CssParser.Show ( CssShow(..) )

data FqFun s
  = FqSelectorFun s
  | FqSomeFun Ident PropVals
  deriving (Show, Eq, Ord, Generic)

instance CssShow s => CssShow (FqFun s) where
  toCssText = \case
    FqSelectorFun sl -> "selector(" <> toCssText sl <> ")"
    FqSomeFun fn args -> toCssText fn <> "(" <> toCssText args <> ")"

data FeatureQuery s
  = FqMediaFeature MediaFeature
  | FqParen (FeatureQuery s)
  | FqBop AndOr (FeatureQuery s) (FeatureQuery s)
  | FqNot (FeatureQuery s)
  | FqApp (FqFun s)
  deriving (Show, Eq, Ord, Generic)

instance CssShow s => CssShow (FeatureQuery s) where
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
    FqApp fn -> toCssText fn

instance Norm (FeatureQuery s) where
  normalize = \case
    FqMediaFeature mf -> FqMediaFeature $ toPlainMf (PropVals $ pure (IntVal 1 Mm)) mf
    FqParen mf@FqMediaFeature {} -> mf
    FqParen fn@FqApp {} -> fn
    FqParen p@FqParen {} -> normalize p
    FqParen p -> FqParen $ normalize p
    FqBop bop (FqParen l) r -> FqBop bop (normalize l) (normalize r)
    FqBop bop l r -> FqBop bop (normalize l) (normalize r)
    FqNot (FqNot fq) -> normalize fq
    FqNot (FqParen fq) -> FqNot $ normalize fq
    FqNot o -> FqNot $ normalize o
    FqApp f -> FqApp f
