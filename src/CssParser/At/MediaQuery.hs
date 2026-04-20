module CssParser.At.MediaQuery where

import CssParser.Ident
import CssParser.Prelude
import CssParser.Show

data PropValType
  = Pixel
  | Dpi
  | Percent
  | K
  | Em
  | Mm
  deriving (Eq, Ord, Show, Enum, Bounded)

instance CssShow PropValType where
  toCssText = \case
    Pixel -> "px"
    Dpi -> "dpi"
    Percent -> "%"
    K -> ""
    Em -> "em"
    Mm -> "mm"

data PropVal
  = IntVal Integer PropValType
  | RatioVal Integer Integer
  | IdentRef Ident
  deriving (Eq, Ord, Show, Generic)

instance CssShow PropVal where
  toCssText = \case
    IntVal i pvt -> numToText i <> toCssText pvt
    RatioVal divisible divider -> numToText divisible <> "/" <> numToText divider
    IdentRef i -> toCssText i

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

{-
<media-query> = <media-condition>
             | [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
<media-type> = <ident>

<media-condition> = <media-not> | <media-in-parens> [ <media-and>* | <media-or>* ]
<media-condition-without-or> = <media-not> | <media-in-parens> <media-and>*
-}
data MediaBoolExpr
  = MediaNot MediaBoolExpr
  | MediaAnd MediaBoolExpr MediaBoolExpr
  | MediaOr MediaBoolExpr MediaBoolExpr
  | MediaFeature MediaFeature
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaBoolExpr where
  toCssText = \case
    MediaNot x -> "not " <> toCssText x
    MediaAnd a b -> toCssText a <> " and " <> toCssText b
    MediaOr a b -> toCssText a <> " or " <> toCssText b
    MediaFeature mf -> "(" <> toCssText mf <> ")"

{-
<media-not> = not <media-in-parens>
<media-and> = and <media-in-parens>
<media-or> = or <media-in-parens>
<media-in-parens> = ( <media-condition> ) | ( <media-feature> )
-}

{-
<media-feature> = [ <mf-plain> | <mf-boolean> | <mf-range> ]
<mf-plain> = <mf-name> : <mf-value>
<mf-boolean> = <mf-name>
-}
data MediaFeature
  = PlainMf Ident PropVal
  | BooleanMf Ident
  | OpenRangeFeature Ident MfRelation PropVal
  | MfClosedRange PropVal MfRelation Ident MfRelation PropVal
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaFeature where
  toCssText = \case
    PlainMf i v -> toCssText i <> ": " <> toCssText v
    BooleanMf i -> toCssText i
    OpenRangeFeature i r v ->
      toCssText i <> " " <> toCssText r <> " " <> toCssText v
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

{-
<mf-range> = <mf-name> <mf-comparison> <mf-value>
           | <mf-value> <mf-comparison> <mf-name>
           | <mf-value> <mf-lt> <mf-name> <mf-lt> <mf-value>
           | <mf-value> <mf-gt> <mf-name> <mf-gt> <mf-value>
<mf-name> = <ident>
<mf-value> = <number> | <dimension> | <ident> | <ratio>
<mf-lt> = '<' '='?
<mf-gt> = '>' '='?
<mf-eq> = '='
<mf-comparison> = <mf-lt> | <mf-gt> | <mf-eq>

reserved for future:
  <general-enclosed> = [ <function-token> <any-value>? ) ] | [ ( <any-value>? ) ]
-}
