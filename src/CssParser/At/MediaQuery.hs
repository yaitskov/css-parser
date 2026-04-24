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

instance CssShow MediaQueryList where
  toCssText (MediaQueryList mqs) =
    "@media " <> intercalate ", " (toCssText <$> mqs)

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

data MediaBoolExpr
  = MediaNot MediaFeature
  | MediaBin AndOr MediaFeature MediaBoolExpr
  | MediaFeature MediaFeature
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaBoolExpr where
  toCssText = \case
    MediaNot x -> "not (" <> toCssText x <> ")"
    MediaBin And x l ->
      toCssText (MediaFeature x) <> " and " <> toCssText l
    MediaBin Or x l ->
      toCssText (MediaFeature x) <> " or " <> toCssText l
    MediaFeature mf -> "(" <> toCssText mf <> ")"

data MediaFeature
  = PlainMf Ident PropVal
  | BooleanMf Ident
  | OpenRangeFeature Ident MfRelation PropVal
  | OpenRangeFeatureFlipped PropVal MfRelation Ident
  | MfClosedRange PropVal MfRelation Ident MfRelation PropVal
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaFeature where
  toCssText = \case
    PlainMf i v -> toCssText i <> ": " <> toCssText v
    BooleanMf i -> toCssText i
    OpenRangeFeature i r v ->
      toCssText i <> " " <> toCssText r <> " " <> toCssText v
    OpenRangeFeatureFlipped v r i ->
      toCssText v <> " " <> toCssText r <> " " <> toCssText i
    MfClosedRange lv lr i rr rv ->
      toCssText lv <> " " <> toCssText lr <> " " <> toCssText i <> toCssText rr <> " " <> toCssText rv

data MfRelation
  = MfEq
  | MfGt
  | MfLt
  | MfGe
  | MfLe
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance CssShow MfRelation where
  toCssText = \case
    MfEq -> "="
    MfGt -> ">"
    MfLt -> "<"
    MfGe -> ">="
    MfLe -> "<="
