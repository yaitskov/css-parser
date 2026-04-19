{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module CssParser.Test.Arbitrary where

import CssParser.File ( CssFile(CssFile) )
import CssParser.Rule
import Data.Text (Text, pack, tails, inits)
import Data.Text qualified as T
import Prelude
import Test.QuickCheck
    ( Gen, Arbitrary(..), arbitraryBoundedEnum, elements, listOf )
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary.Generic
    ( GenericArbitrary(GenericArbitrary), genericArbitrary, genericShrink )

arbitraryIdent :: Gen Text
arbitraryIdent = arbitraryText fl nl
  where
    fl = ['a' .. 'z'] <> ['A' .. 'Z'] <> "_"
    nl = fl <> ['-', '0' .. '9']

arbitraryText :: String -> String -> Gen Text
arbitraryText fl nl = do
  fc <- elements fl
  pack . (fc :) <$> listOf (elements nl)

arbitraryLanguages :: [Text]
arbitraryLanguages = ["af", "af-ZA", "ar", "ar-AE", "ar-BH", "ar-DZ", "ru"]

shrinkText :: Text -> [Text]
shrinkText = liftA2 (zipWith (<>)) inits (tails . T.drop 1)

shrinkIdent :: Text -> [Text]
shrinkIdent t
    | T.length t < 2 = []
    | otherwise = shrinkText t

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
  arbitrary = Language <$> arbitraryText ['a'..'z'] ['a'..'z']
  shrink (Language a) = Language <$> shrinkIdent a


deriving via (GenericArbitrary PropertyName) instance Arbitrary PropertyName
deriving via (GenericArbitrary CssFile) instance Arbitrary CssFile
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
