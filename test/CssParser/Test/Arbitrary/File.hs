{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.File where

import CssParser.File ( CssFile, FileHeader )
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Rule ()

deriving via (GenericArbitrary FileHeader) instance Arbitrary FileHeader
deriving via (GenericArbitrary CssFile) instance Arbitrary CssFile
