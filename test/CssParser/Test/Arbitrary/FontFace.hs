{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.FontFace where

import CssParser.At.FontFace
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import CssParser.Test.Arbitrary.At ()

deriving via (GenericArbitrary CommaSeparatedList) instance Arbitrary CommaSeparatedList
deriving via (GenericArbitrary FontFace) instance Arbitrary FontFace
