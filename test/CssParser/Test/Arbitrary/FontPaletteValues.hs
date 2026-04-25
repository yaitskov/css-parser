{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.FontPaletteValues where

import CssParser.At.FontPaletteValues ( FontPaletteValues )
import CssParser.Test.Arbitrary
    ( Arbitrary, Gen(MkGen), GenericArbitrary(GenericArbitrary) )
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.At ()

deriving via (GenericArbitrary FontPaletteValues) instance Arbitrary FontPaletteValues
