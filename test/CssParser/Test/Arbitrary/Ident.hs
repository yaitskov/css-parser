{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CssParser.Test.Arbitrary.Ident where

import CssParser.Ident
import CssParser.Test.Arbitrary

instance Arbitrary Ident where
  arbitrary = Ident <$> arbitraryIdent
  shrink (Ident a) = Ident <$> shrinkIdent a

deriving via (GenericArbitrary PropertyName) instance Arbitrary PropertyName
deriving via (GenericArbitrary TagName) instance Arbitrary TagName
deriving via (GenericArbitrary Namespace) instance Arbitrary Namespace
deriving via (GenericArbitrary AttrName) instance Arbitrary AttrName
