{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Supports where

import CssParser.At.Supports
import CssParser.Norm

import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Media ()


instance Arbitrary FeatureQuery where
  arbitrary = normUntilConst <$> genericArbitrary
  shrink = normUntilConst <$> genericShrink
