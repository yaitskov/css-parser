{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Value where

import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()

instance Norm Unsigned where
  normalize = abs

instance Arbitrary Unsigned where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink
instance Arbitrary Url where
  arbitrary = pure $ Url "https://ooo.com/aoeu/style.css"

deriving via (GenericArbitrary Ratio) instance Arbitrary Ratio
deriving via (GenericArbitrary PropVals) instance Arbitrary PropVals
deriving via (GenericArbitrary PropVal) instance Arbitrary PropVal
deriving via (GenericArbitrary PropValType) instance Arbitrary PropValType
