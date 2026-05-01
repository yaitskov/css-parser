{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module CssParser.Test.Arbitrary.Rule where

import CssParser.At.Supports ( FqFun )
import CssParser.Ident ( TagName(NoTag, AsteriskTag) )
import CssParser.Norm ( normUntilConst, Norm(..) )
import CssParser.Prelude
import CssParser.Rule
import CssParser.Rule.Pseudo ( Language(..), Nth, PseudoElement )
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.At ()
import CssParser.Test.Arbitrary.Container ()
import CssParser.Test.Arbitrary.FontFace ()
import CssParser.Test.Arbitrary.FontFeatureValues ()
import CssParser.Test.Arbitrary.FontPaletteValues ()
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Media ()
import CssParser.Test.Arbitrary.MonoPair ()
import Data.List ( null )
instance Arbitrary Hash where
    arbitrary = Hash <$> arbitraryIdent
    shrink (Hash a) = Hash <$> shrinkIdent a

instance Arbitrary Language where
  arbitrary = Language <$> elements ["en", "af-ZA", "ar", "de", "ar-BH", "pl", "ru"]

isPartialTagSelector :: TagSelector -> Bool
isPartialTagSelector TagSelector {..} =
  tagName == NoTag && null tagAttrs && isNothing tagId && null tagClasses

instance Arbitrary TagSelector where
  arbitrary = do
    ts <- genericArbitrary
    if isPartialTagSelector ts
      then pure ts { tagName = AsteriskTag }
      else pure ts
  shrink = filter (not . isPartialTagSelector) . genericShrink

instance Norm Selector where
  normalize = \case
    Selector _ fts ots -> Selector Nothing fts ots
    PeSelector _ fts ots pe -> PeSelector Nothing fts ots pe
    PeSelectorOnly pe -> PeSelectorOnly pe

instance Arbitrary Selector where
  arbitrary = normalize <$> genericArbitrary
  shrink x = fmap normalize (genericShrink x)

deriving via (GenericArbitrary TagRelation) instance Arbitrary TagRelation
deriving via (GenericArbitrary Class) instance Arbitrary Class
deriving via (GenericArbitrary Attr) instance Arbitrary Attr

deriving via (GenericArbitrary CssRule) instance Arbitrary CssRule
deriving via (GenericArbitrary CssRuleBodyItem) instance Arbitrary CssRuleBodyItem
deriving via (GenericArbitrary Nth) instance Arbitrary Nth
deriving via (GenericArbitrary AttrOp) instance Arbitrary AttrOp
deriving via (GenericArbitrary PseudoElement) instance Arbitrary PseudoElement


deriving via (GenericArbitrary (FqFun SelectorList)) instance Arbitrary (FqFun SelectorList)

instance Arbitrary FeatureQuery where
  arbitrary = normUntilConst <$> genericArbitrary
  shrink = normUntilConst <$> genericShrink
