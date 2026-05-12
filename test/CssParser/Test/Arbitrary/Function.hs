{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Function where

import CssParser.At.Function ( FunArg, ConstEntry, Function )
import CssParser.Test.Arbitrary
    ( genericShrink,
      Arbitrary(..),
      Gen(MkGen),
      genericArbitrary,
      GenericArbitrary(GenericArbitrary) )
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import CssParser.Test.Arbitrary.At ()

deriving via (GenericArbitrary FunArg) instance Arbitrary FunArg
deriving via (GenericArbitrary ConstEntry) instance Arbitrary ConstEntry

instance Arbitrary r => Arbitrary (Function r) where
  arbitrary = genericArbitrary
  shrink = genericShrink
