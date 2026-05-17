{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.At where

import CssParser.At.Keyframe
import CssParser.At.Page
import CssParser.Ident
import CssParser.Norm ( Norm(..) )
import CssParser.Rule.Pseudo
    ( AtomicPseudoClass(Blank), BrowserSpecificIdent(..) )
import CssParser.Rule.Value ( Source(..) )
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import Data.Text (isPrefixOf)
import Data.Text qualified as T

browserPrefixes :: [Text]
browserPrefixes =  T.words "-moz- -ms- -webkit- -apple- -o- $"

instance Arbitrary BrowserSpecificIdent where
  arbitrary =
    prependIfMissing <$> elements browserPrefixes <*> arbitrary
    where
      prependIfMissing pre = \case
        o@(Ident x)
          | any (`isPrefixOf` x) browserPrefixes ->
            BrowserSpecificIdent o
          | otherwise ->
            BrowserSpecificIdent (Ident $ pre <> x)

  shrink = filter skipBadUpc . genericShrink
    where
      skipBadUpc (BrowserSpecificIdent (Ident x)) =  T.length x > 9

deriving via (GenericArbitrary AtomicPseudoClass) instance Arbitrary AtomicPseudoClass

instance Arbitrary Charset where
  arbitrary = Charset <$> elements ["UTF-8", "iso-8859-15"]

instance Arbitrary Source where
  arbitrary =
    oneof
    [ UrlSource <$> arbitrary
    , StrSource <$> arbitraryWord
    ]
deriving via (GenericArbitrary LayerName) instance Arbitrary LayerName

deriving via (GenericArbitrary PageMargin) instance Arbitrary PageMargin

deriving via (GenericArbitrary PageName) instance Arbitrary PageName
deriving via (GenericArbitrary PageSelectorList) instance Arbitrary PageSelectorList

instance Norm PageSelector where
  normalize = \case
    PageSelector Nothing [] -> PageSelector Nothing [Blank]
    o -> o

instance Arbitrary PageSelector where
  arbitrary = normalize <$> genericArbitrary
  shrink = filter (/= PageSelector Nothing []) . genericShrink

deriving via (GenericArbitrary KeyframeSet) instance Arbitrary KeyframeSet
deriving via (GenericArbitrary KeyframeSetName) instance Arbitrary KeyframeSetName
deriving via (GenericArbitrary Keyframe) instance Arbitrary Keyframe
deriving via (GenericArbitrary KeyframeAdr) instance Arbitrary KeyframeAdr
deriving via (GenericArbitrary PropEntry) instance Arbitrary PropEntry
