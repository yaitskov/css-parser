{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Media where

import CssParser.At.MediaQuery
import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()

instance Norm MediaFeature where
  normalize = \case
    OpenRangeFeatureFlipped v@IdentRef {} r i -> OpenRangeFeature i (flipRel r) v
    OpenRangeFeatureFlipped v@VarRef {} r i -> OpenRangeFeature i (flipRel r) v
    MfClosedRange lv MfEq i _ _ -> PlainMf i lv
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

deriving via (GenericArbitrary AndOr) instance Arbitrary AndOr

deriving via (GenericArbitrary MediaBoolExpr) instance Arbitrary MediaBoolExpr
deriving via (GenericArbitrary MfRelation) instance Arbitrary MfRelation
deriving via (GenericArbitrary MtModifier) instance Arbitrary MtModifier
deriving via (GenericArbitrary MediaType) instance Arbitrary MediaType


deriving via (GenericArbitrary MediaQuery) instance Arbitrary MediaQuery
deriving via (GenericArbitrary MediaQueryList) instance Arbitrary MediaQueryList
