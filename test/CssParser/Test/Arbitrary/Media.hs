{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Media where

import CssParser.At.CustomMedia ( CustomMediaQuery(..) )
import CssParser.At.MediaQuery
import CssParser.Norm ( Norm(..) )
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()

stripTopPar :: PropVal -> PropVal
stripTopPar = \case
  CalcFun (CalcCe NoFn x) -> CalcFun (CalcCe CalcFn x)
  o -> o

instance Norm MediaFeature where
  normalize = \case
    OpenRangeFeatureFlipped v@IdentRef {} r i -> OpenRangeFeature i (flipRel r) v
    OpenRangeFeatureFlipped v@VarRef {} r i -> OpenRangeFeature i (flipRel r) v
    OpenRangeFeatureFlipped x r i -> OpenRangeFeatureFlipped (stripTopPar x) r i
    MfClosedRange lv MfEq i _ _ -> PlainMf i $ PropVals (pure lv) Nothing
    MfClosedRange lv l i r rv -> MfClosedRange (stripTopPar lv) l i r rv
    o -> o

instance Arbitrary MediaFeature where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

flipRel :: MfRelation -> MfRelation
flipRel = \case
  MfEq -> MfEq
  MfGt -> MfLt
  MfLt -> MfGt
  MfGe -> MfLe
  MfLe -> MfGe

deriving via (GenericArbitrary BinOp) instance Arbitrary BinOp
deriving via (GenericArbitrary MediaCondition) instance Arbitrary MediaCondition
deriving via (GenericArbitrary MfRelation) instance Arbitrary MfRelation
deriving via (GenericArbitrary MtModifier) instance Arbitrary MtModifier
deriving via (GenericArbitrary MediaType) instance Arbitrary MediaType

instance Norm MediaQuery where
  normalize = \case
    MediaQueryConditionOnly mc -> MediaQueryConditionOnly $ stripParens mc
    MediaQueryWithMt md mt mc -> MediaQueryWithMt md mt (stripParens <$> mc)

instance Arbitrary MediaQuery where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x

deriving via (GenericArbitrary MediaQueryList) instance Arbitrary MediaQueryList

instance Norm CustomMediaQuery where
  normalize = \case
    CustomMediaQuery (MediaQueryList []) -> CustomMediaFlag False
    o -> o

instance Arbitrary CustomMediaQuery where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$> genericShrink x
