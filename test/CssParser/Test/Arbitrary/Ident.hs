{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CssParser.Test.Arbitrary.Ident where

import CssParser.Ident
import CssParser.Descriptor
import CssParser.Test.Arbitrary
import Data.Text as T

instance Arbitrary Ident where
  arbitrary = Ident <$> arbitraryIdent
  shrink (Ident a) = Ident <$> shrinkIdent a

instance Arbitrary Descriptor where
  arbitrary = (\case
                  BrowserSpecificDescriptor Na i -> BrowserSpecificDescriptor Opera i
                  o -> o) <$> genericArbitrary

deriving via (GenericArbitrary KnownDescriptor) instance Arbitrary KnownDescriptor
deriving via (GenericArbitrary BrowserPrefix) instance Arbitrary BrowserPrefix
deriving via (GenericArbitrary Var) instance Arbitrary Var
deriving via (GenericArbitrary PropertyName) instance Arbitrary PropertyName

deriving via (GenericArbitrary Namespace) instance Arbitrary Namespace
deriving via (GenericArbitrary AttrName) instance Arbitrary AttrName
deriving via (GenericArbitrary CustomSelectorName) instance Arbitrary CustomSelectorName

instance Arbitrary TagName where
  arbitrary = frequency
    [ (1, pure NoTag)
    , (2, pure AsteriskTag)
    , (3, pure AmpersandTag)
    , (6, TagName . Ident <$> elements (T.words "div p b i a span s"))
    ]
  shrink _ = []
