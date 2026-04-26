module CssParser.At.Container where

import CssParser.At.MediaQuery
import CssParser.Ident
import CssParser.Prelude
import CssParser.Show

instance ShowSpaceBetween Ident ContainerQuery where
  cssSpace _ _ = " "

instance ShowSpaceBetween (These Ident ContainerQuery) (These Ident ContainerQuery) where
  cssSpace _ _ = ", "

newtype ContainerQueryMap
  = ContainerQueryMap (NonEmpty (These Ident ContainerQuery))
  deriving (Show, Eq, Ord, Generic)

instance CssShow ContainerQueryMap where
  toCssText (ContainerQueryMap cqm) = toCssText cqm


data CqOp
  = CqOpFeature MediaFeature
  | CqApp Ident ContainerQuery
  deriving (Show, Eq, Ord, Generic)

instance CssShow CqOp where
  toCssText = \case
    CqOpFeature f -> "(" <> toCssText f <> ")"
    CqApp f (CqFeature (AsIs x@CqOpFeature {})) ->
      toCssText f <> toCssText x
    CqApp f (CqFeature x) ->
      toCssText f <> "(" <> toCssText x <> ")"
    CqApp f args ->
      toCssText f <> "(" <> toCssText args <> ")"

instance ShowParenthesis ContainerQuery CqOp where
  left _ _ = ""
  right _ _ = ""

data ContainerQuery
  = CqBin AndOr (Not ContainerQuery CqOp) ContainerQuery
  | CqFeature (Not ContainerQuery CqOp)
  deriving (Show, Eq, Ord, Generic)

instance CssShow ContainerQuery where
  toCssText = \case
    CqBin bop x l ->
      toCssText (CqFeature x) <> toCssText bop  <> toCssText l
    CqFeature mf -> toCssText mf
