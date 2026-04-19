{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Rule where

import CssParser.FixRule
import CssParser.Rule
import CssParser.Test.Arbitrary


instance Arbitrary Hash where
    arbitrary = Hash <$> arbitraryIdent
    shrink (Hash a) = Hash <$> shrinkIdent a

instance Arbitrary TagSelector where
  arbitrary = do
    ts <- genericArbitrary
    if ts == nullTagSelector
      then pure ts { tagName = AsteriskTag }
      else pure ts
  shrink = filter (/= nullTagSelector) . genericShrink

instance Arbitrary Ident where
  arbitrary = Ident <$> arbitraryIdent
  shrink (Ident a) = Ident <$> shrinkIdent a

instance Arbitrary Language where
  arbitrary = Language <$> elements ["en", "af-ZA", "ar", "de", "ar-BH", "pl", "ru"]

deriving via (GenericArbitrary PropertyName) instance Arbitrary PropertyName

deriving via (GenericArbitrary Selector) instance Arbitrary Selector
deriving via (GenericArbitrary TagRelation) instance Arbitrary TagRelation
deriving via (GenericArbitrary TagName) instance Arbitrary TagName
deriving via (GenericArbitrary Namespace) instance Arbitrary Namespace
deriving via (GenericArbitrary Class) instance Arbitrary Class
deriving via (GenericArbitrary Attr) instance Arbitrary Attr
deriving via (GenericArbitrary AttrName) instance Arbitrary AttrName
deriving via (GenericArbitrary CssRule) instance Arbitrary CssRule
deriving via (GenericArbitrary CssRuleBodyItem) instance Arbitrary CssRuleBodyItem
deriving via (GenericArbitrary Nth) instance Arbitrary Nth
deriving via (GenericArbitrary AttrOp) instance Arbitrary AttrOp
deriving via (GenericArbitrary PseudoElement) instance Arbitrary PseudoElement

instance Arbitrary AtomicPseudoClass where
  arbitrary = arbitraryBoundedEnum
