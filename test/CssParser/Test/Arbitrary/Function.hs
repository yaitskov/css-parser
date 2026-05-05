{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Function where

import CssParser.At.Function as F
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import CssParser.Test.Arbitrary.At ()

deriving via (GenericArbitrary CssType) instance Arbitrary CssType
deriving via (GenericArbitrary CssLeafType) instance Arbitrary CssLeafType
deriving via (GenericArbitrary AtomicCssType) instance Arbitrary AtomicCssType
deriving via (GenericArbitrary FunArg) instance Arbitrary FunArg
deriving via (GenericArbitrary ConstEntry) instance Arbitrary ConstEntry

instance Arbitrary r => Arbitrary (F.Function r) where
  arbitrary = genericArbitrary
  shrink = genericShrink
