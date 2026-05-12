{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Container where

import CssParser.At.Container
    ( ContainerQuery, CqOp(CqOpFeature), ContainerQueryMap(..) )
import CssParser.At.MediaQuery ( Not, toPlainMf )
import CssParser.Norm ( Norm(..) )
import CssParser.Rule.Value ( PropVal(IntVal), PropVals(PropVals) )
import CssParser.Rule.TypedNum
    ( TypedNum(TypedNum), RawNum(RawNum), PropValType(Mm) )
import CssParser.Test.Arbitrary
    ( Applicative(pure),
      Maybe(Nothing),
      (<$>),
      genericShrink,
      Arbitrary(..),
      Gen(MkGen),
      genericArbitrary,
      GenericArbitrary(GenericArbitrary) )
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Media ()

instance Norm CqOp where
  normalize = \case
    CqOpFeature mf ->
      let zero = IntVal (TypedNum (RawNum "0") Mm) in
        CqOpFeature (toPlainMf (PropVals (pure zero) Nothing) mf)
    o -> o

deriving via (GenericArbitrary (Not ContainerQuery CqOp)) instance Arbitrary (Not ContainerQuery CqOp)
deriving via (GenericArbitrary ContainerQuery) instance Arbitrary ContainerQuery

instance Arbitrary CqOp where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink

deriving via (GenericArbitrary ContainerQueryMap) instance Arbitrary ContainerQueryMap
