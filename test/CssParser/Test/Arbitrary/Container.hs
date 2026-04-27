{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Container where

import CssParser.At.Container
import CssParser.At.MediaQuery

import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Media ()

instance Norm CqOp where
  normalize = \case
    CqOpFeature mf -> CqOpFeature (n mf)
    o -> o
    where
    n = \case
      BooleanMf pn -> PlainMf pn . PropVals $ pure (IntVal 0 Mm)
      OpenRangeFeature pn _ pv -> PlainMf pn . PropVals $ pure pv
      OpenRangeFeatureFlipped pv _ pn -> PlainMf pn . PropVals $ pure pv
      MfClosedRange pv _  pn _ _ -> PlainMf pn . PropVals $ pure pv
      o -> o

deriving via (GenericArbitrary (Not ContainerQuery CqOp)) instance Arbitrary (Not ContainerQuery CqOp)
deriving via (GenericArbitrary ContainerQuery) instance Arbitrary ContainerQuery

instance Arbitrary CqOp where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink

deriving via (GenericArbitrary ContainerQueryMap) instance Arbitrary ContainerQueryMap
