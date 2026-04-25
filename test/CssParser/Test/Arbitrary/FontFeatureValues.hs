{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.FontFeatureValues where

import CssParser.Prelude
import CssParser.At.FontFeatureValues
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import CssParser.Test.Arbitrary.At ()

deriving via (GenericArbitrary IdentList) instance Arbitrary IdentList
deriving via (GenericArbitrary FontFeatureValuesSubBlock) instance Arbitrary FontFeatureValuesSubBlock
deriving via (GenericArbitrary FontFeatureValues) instance Arbitrary FontFeatureValues
