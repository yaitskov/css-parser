{-# LANGUAGE UndecidableInstances #-}
module CssParser.MonoPair where

import CssParser.Prelude
import CssParser.Show

data MonoPair a
  = EmptyPair
  | HalfPair a
  | FullPair a a
  deriving (Show, Ord, Eq, Generic)

instance
  ( ShowSpaceBetween (MonoPair a) a
  , ShowParenthesis (MonoPair a) a
  , CssShow a
  ) => CssShow (MonoPair a) where
  toCssText = \case
    EmptyPair -> ""
    HalfPair x ->  wrap (toCssText x)
    FullPair x y -> wrap (toCssText x <> cssSpace (MonoPair a) a <> toCssText y)
    where
      wrap x = left (MonoPair a) a <> x <> right (MonoPair a) a
