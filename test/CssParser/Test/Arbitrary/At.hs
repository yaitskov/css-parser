{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.At where

import CssParser.At
import CssParser.At.Import
import CssParser.At.Layer
import CssParser.At.Namespace
import CssParser.At.Page
import CssParser.Norm
import CssParser.Rule.Value
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()


instance Arbitrary Charset where
  arbitrary = Charset <$> elements ["UTF-8", "iso-8859-15"]

instance Arbitrary Url where
  arbitrary = pure $ Url "https://ooo.com/aoeu/style.css"

instance Arbitrary Source where
  arbitrary =
    oneof
    [ UrlSource <$> arbitrary
    , StrSource <$> arbitraryWord
    ]

deriving via (GenericArbitrary LayerName) instance Arbitrary LayerName
deriving via (GenericArbitrary LayerStmt) instance Arbitrary LayerStmt
deriving via (GenericArbitrary Import) instance Arbitrary Import

deriving via (GenericArbitrary PageMargin) instance Arbitrary PageMargin
deriving via (GenericArbitrary PseudoPage) instance Arbitrary PseudoPage
deriving via (GenericArbitrary PageName) instance Arbitrary PageName
deriving via (GenericArbitrary PageSelectorList) instance Arbitrary PageSelectorList

instance Norm PageSelector where
  normalize = \case
    PageSelector Nothing [] -> PageSelector Nothing [BlankPp]
    o -> o

instance Arbitrary PageSelector where
  arbitrary = normalize <$> genericArbitrary
  shrink = filter (/= PageSelector Nothing []) . genericShrink

deriving via (GenericArbitrary Namespace) instance Arbitrary Namespace
