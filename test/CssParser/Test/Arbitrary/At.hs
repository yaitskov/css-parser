{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.At where

import CssParser.At
import CssParser.At.Import
import CssParser.Test.Arbitrary

instance Arbitrary Charset where
  arbitrary = Charset <$> elements ["UTF-8", "iso-8859-15"]

instance Arbitrary Url where
  arbitrary = pure $ Url "https://ooo.com/aoeu/style.css"

instance Arbitrary ImportSource where
  arbitrary =
    oneof
    [ ImportSourceUrl <$> arbitrary
    , ImportSourceStr <$> arbitraryWord
    ]

deriving via (GenericArbitrary Import) instance Arbitrary Import
