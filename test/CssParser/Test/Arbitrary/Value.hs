{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Value where

import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary

instance Norm Unsigned where
  normalize = abs

instance Arbitrary Unsigned where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink

deriving via (GenericArbitrary Ratio) instance Arbitrary Ratio
