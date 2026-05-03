module CssParser.At.MediaQuery where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Rule.Value
import CssParser.Show

newtype MediaType = MediaType Ident deriving (Eq, Ord, Generic) deriving newtype (Show, CssShow)

data MtModifier = MtNot | MtOnly deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance CssShow MtModifier where
  toCssText = \case
    MtNot -> "not"
    MtOnly -> "only"

newtype MediaQueryList = MediaQueryList [MediaQuery] deriving (Show, Eq, Ord, Generic)

instance ShowSpaceBetween MediaQuery MediaQuery where
  cssSpace _ _ = ", "

instance CssShow MediaQueryList where
  toCssText (MediaQueryList mqs) = "@media " <> toCssText mqs

data MediaQuery
  = MediaQueryConditionOnly MediaBoolExpr
  | MediaQueryWithMt (Maybe MtModifier) MediaType (Maybe MediaBoolExpr)
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaQuery where
  toCssText = \case
    MediaQueryConditionOnly mbe -> toCssText mbe
    MediaQueryWithMt mtMod mt mbe ->
      maybe "" ((<> " ") . toCssText) mtMod <>
      toCssText mt <>
      maybe "" ((" and " <>) . toCssText) mbe

data AndOr = And | Or deriving (Show, Eq, Ord, Generic)

instance CssShow AndOr where
  toCssText = \case
    And -> " and "
    Or -> " or "

data Not p a = Not a | AsIs a deriving (Show, Eq, Ord, Generic)

instance (ShowParenthesis p a, CssShow a) => CssShow (Not p a) where
  toCssText = \case
    Not x -> "not " <> left p a <> toCssText x <> right p a
    AsIs x -> left p a <> toCssText x <> right p a

instance ShowParenthesis MediaBoolExpr MediaFeature where
  left _ _ = "("
  right _ _ = ")"

data MediaBoolExpr
  = MediaBin AndOr (Not MediaBoolExpr MediaFeature) MediaBoolExpr
  | MediaFeature (Not MediaBoolExpr MediaFeature)
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaBoolExpr where
  toCssText = \case
    MediaBin bop x l ->
      toCssText (MediaFeature x) <> toCssText bop  <> toCssText l
    MediaFeature mf -> toCssText mf

data MediaFeature
  = PlainMf PropertyName PropVals
  | BooleanMf PropertyName
  | OpenRangeFeature PropertyName MfRelation PropVal
  | OpenRangeFeatureFlipped PropVal MfRelation PropertyName
  | MfClosedRange PropVal MfRelation PropertyName MfRelation PropVal
  deriving (Show, Eq, Ord, Generic)

toPlainMf :: PropVals -> MediaFeature -> MediaFeature
toPlainMf pvs = \case
  BooleanMf pn -> PlainMf pn pvs
  OpenRangeFeature pn _ pv -> PlainMf pn $ PropVals (pure pv) Nothing
  OpenRangeFeatureFlipped pv _ pn -> PlainMf pn $ PropVals (pure pv) Nothing
  MfClosedRange pv _  pn _ _ -> PlainMf pn $ PropVals (pure pv) Nothing
  o -> o

instance CssShow MediaFeature where
  toCssText = \case
    PlainMf i v -> toCssText i <> ": " <> toCssText v
    BooleanMf i -> toCssText i
    OpenRangeFeature i r v ->
      toCssText i <> toCssText r <> toCssText v
    OpenRangeFeatureFlipped v r i ->
      toCssText v <> toCssText r <> toCssText i
    MfClosedRange lv lr i rr rv ->
      toCssText lv <> toCssText lr <> toCssText i <> toCssText rr <> toCssText rv

data MfRelation
  = MfEq
  | MfGt
  | MfLt
  | MfGe
  | MfLe
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance CssShow MfRelation where
  toCssText = \case
    MfEq -> " = "
    MfGt -> " > "
    MfLt -> " < "
    MfGe -> " >= "
    MfLe -> " <= "
