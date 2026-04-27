{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CssParser.Test.Arbitrary.MonoPair where

import CssParser.MonoPair
import CssParser.Test.Arbitrary

instance Arbitrary a => Arbitrary (MonoPair a) where
  arbitrary =
    frequency
    [ (1, pure EmptyPair)
    , (4, HalfPair <$> arbitrary)
    , (3, FullPair <$> arbitrary <*> arbitrary)
    ]
