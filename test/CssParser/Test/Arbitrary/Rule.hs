{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.Rule where

import CssParser.Ident
import CssParser.FixRule ( nullTagSelector )
import CssParser.Rule
import CssParser.Rule.Pseudo
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Media ()


instance Arbitrary Hash where
    arbitrary = Hash <$> arbitraryIdent
    shrink (Hash a) = Hash <$> shrinkIdent a

instance Arbitrary Language where
  arbitrary = Language <$> elements ["en", "af-ZA", "ar", "de", "ar-BH", "pl", "ru"]

instance Arbitrary TagSelector where
  arbitrary = do
    ts <- genericArbitrary
    if ts == nullTagSelector
      then pure ts { tagName = AsteriskTag }
      else pure ts
  shrink = filter (/= nullTagSelector) . genericShrink

deriving via (GenericArbitrary Selector) instance Arbitrary Selector
deriving via (GenericArbitrary TagRelation) instance Arbitrary TagRelation
deriving via (GenericArbitrary Class) instance Arbitrary Class
deriving via (GenericArbitrary Attr) instance Arbitrary Attr

deriving via (GenericArbitrary CssRule) instance Arbitrary CssRule
deriving via (GenericArbitrary CssRuleBodyItem) instance Arbitrary CssRuleBodyItem
deriving via (GenericArbitrary Nth) instance Arbitrary Nth
deriving via (GenericArbitrary AttrOp) instance Arbitrary AttrOp
deriving via (GenericArbitrary PseudoElement) instance Arbitrary PseudoElement

instance Arbitrary AtomicPseudoClass where
  arbitrary = arbitraryBoundedEnum
