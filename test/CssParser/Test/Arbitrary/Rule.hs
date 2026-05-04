{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module CssParser.Test.Arbitrary.Rule where

import CssParser.At.Supports ( FqFun )
import CssParser.At.Supports qualified as S
import CssParser.Ident ( TagName(NoTag, AsteriskTag) )
import CssParser.Norm ( normUntilConst, Norm(..) )
import CssParser.Prelude
import CssParser.Rule
import CssParser.Rule.Pseudo ( Language(..), Nth, PseudoElement )
import CssParser.Rule.Value
import CssParser.At.Import
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


instance Norm CssRuleBodyItem where
  normalize = \case
    CssEnumLeaf pn (PropValsList (x :| [])) -> CssLeafRule pn x
    o -> o

deriving via (GenericArbitrary CssRule) instance Arbitrary CssRule

instance Arbitrary CssRuleBodyItem where
  arbitrary = normalize <$> genericArbitrary
  shrink x = normalize <$>  genericShrink x

deriving via (GenericArbitrary Nth) instance Arbitrary Nth
deriving via (GenericArbitrary AttrOp) instance Arbitrary AttrOp
deriving via (GenericArbitrary PseudoElement) instance Arbitrary PseudoElement
deriving via (GenericArbitrary CompositePe) instance Arbitrary CompositePe
deriving via (GenericArbitrary PseudeTagSelector) instance Arbitrary PseudeTagSelector


deriving via (GenericArbitrary (FqFun SelectorList)) instance Arbitrary (FqFun SelectorList)

instance Arbitrary FeatureQuery where
  arbitrary = normUntilConst <$> genericArbitrary
  shrink = normUntilConst <$> genericShrink

expandToParen :: FeatureQuery ->  FeatureQuery
expandToParen = \case
  S.FqParen x -> expandToParen x
  o -> o

instance Norm (Import SelectorList) where
  normalize = \case
    ImportUrlLayer src ln mfq mqs -> ImportUrlLayer src ln (expandToParen <$> mfq) mqs
    ImportUrlSupports src mfq mqs -> ImportUrlSupports src (expandToParen <$> mfq) mqs
    o -> o

instance Arbitrary (Import SelectorList) where
  arbitrary = normalize <$> genericArbitrary
  shrink = normalize <$> genericShrink
