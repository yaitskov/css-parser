module CssParser.At.MediaQuery where

import CssParser.Ident ( PropertyName )
import CssParser.Prelude
import CssParser.Rule.Value
import CssParser.Show
    ( CssShow(..), ShowParenthesis(..), ShowSpaceBetween(..) )
import Expression.Reorder

data MediaType
  = AllMt
  | Print
  | Screen
  | Tty
  | Tv
  | Projection
  | Handheld
  | Braille
  | Embossed
  | Aural
  | Speech
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance CssShow MediaType where
  toCssText = \case
    AllMt -> "all"
    Print -> "print"
    Screen -> "screen"
    Tty -> "tty"
    Tv -> "tv"
    Projection -> "projection"
    Handheld -> "handheld"
    Braille -> "braille"
    Embossed -> "embossed"
    Aural -> "aural"
    Speech -> "speech"

data MtModifier = MtNot | MtOnly deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance CssShow MtModifier where
  toCssText = \case
    MtNot -> "not "
    MtOnly -> "only "

newtype MediaQueryList = MediaQueryList [MediaQuery] deriving (Show, Eq, Ord, Generic)

instance ShowSpaceBetween MediaQuery MediaQuery where
  cssSpace _ _ = ", "

instance CssShow MediaQueryList where
  toCssText (MediaQueryList mqs) = toCssText mqs

data MediaQuery
  = MediaQueryConditionOnly MediaCondition
  | MediaQueryWithMt (Maybe MtModifier) MediaType (Maybe MediaCondition)
  deriving (Show, Eq, Ord, Generic)

instance CssShow MediaQuery where
  toCssText = \case
    MediaQueryConditionOnly mbe -> toCssText mbe
    MediaQueryWithMt mtMod mt mbe ->
      toCssText mtMod <>
      toCssText mt <>
      maybe "" ((" and " <>) . toCssText) mbe

data BinOp = Or | And deriving (Show, Eq, Ord, Generic)

instance CssShow BinOp where
  toCssText = \case
    And -> " and "
    Or -> " or "

data Not p a = Not a | AsIs a deriving (Show, Eq, Ord, Generic)

instance (ShowParenthesis p a, CssShow a) => CssShow (Not p a) where
  toCssText = \case
    Not x -> "not " <> left p a <> toCssText x <> right p a
    AsIs x -> left p a <> toCssText x <> right p a

data MediaCondition where
  NotMc :: MediaCondition -> MediaCondition
  BopMc :: MediaCondition ->
             BinOp ->
             MediaCondition ->
             MediaCondition
  ParenMc :: MediaCondition -> MediaCondition
  FeatureMc :: MediaFeature -> MediaCondition
  deriving (Show, Eq, Ord, Generic)

mkBopMcTree :: BinOp -> NonEmpty MediaCondition -> MediaCondition
mkBopMcTree bop = \case
  x :| [] -> x
  x :| [y] -> BopMc x bop y
  x :| y : l -> BopMc x bop (mkBopMcTree bop $ y :| l)

instance HasFixity BinOp where
  fixityOf = \case
    Or -> Fixity AssocLeft 1
    And -> Fixity AssocLeft 2

instance SyntaxTree MediaCondition String where
  reorderChildren = \case
    BopMc l op r -> BopMc <$> reorder l <*> pure op <*> reorder r
    ParenMc x -> ParenMc <$> reorder x
    NotMc x -> NotMc <$> reorder x
    o -> pure o
  structureOf = \case
    BopMc l op r -> NodeInfix (fixityOf op) l r (`BopMc` op)
    NotMc x -> NodePrefix 3 x NotMc
    _ -> NodeLeaf
  makeError err _ = show err

instance CssShow MediaCondition where
  toCssText = \case
    NotMc mc@FeatureMc {} -> "not " <> toCssText mc
    NotMc mc@ParenMc {} -> "not " <> toCssText mc
    NotMc mc -> "not (" <> toCssText mc <> ")"
    BopMc l op r -> showBopOperand op l <> toCssText op <> showBopOperandR r
    ParenMc mc -> "(" <> toCssText mc <> ")"
    FeatureMc mf -> "(" <> toCssText mf <> ")"

showBopOperand :: BinOp -> MediaCondition -> LText
showBopOperand pop = \case
  BopMc l cop r
    | cop /= pop -> "(" <> showBopOperand cop l <> toCssText cop <> showBopOperandR r <> ")"
    | otherwise -> showBopOperand cop l <> toCssText cop <> showBopOperandR r
  o -> toCssText o

showBopOperandR :: MediaCondition -> LText
showBopOperandR = \case
  BopMc l cop r -> "(" <> showBopOperand cop l <> toCssText cop <> showBopOperandR r <> ")"
  o -> toCssText o

instance HasParens MediaCondition where
  stripParens = \case
    x@FeatureMc {} -> x
    ParenMc x -> stripParens x
    BopMc l op r -> BopMc (stripParens l) op (stripParens r)
    NotMc x -> NotMc $ stripParens x

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
