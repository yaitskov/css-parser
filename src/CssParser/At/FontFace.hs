{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module CssParser.At.FontFace where

import CssParser.Rule.Value
import CssParser.At.Keyframe
import CssParser.Prelude
import CssParser.Show

newtype CommaSeparatedList
  = CommaSeparatedList (NonEmpty PropVals)
  deriving (Show, Eq, Ord, Generic)

instance ShowSpaceBetween CommaSeparatedList CommaSeparatedList where
  cssSpace _ _ = "; "
type SrcVal = CommaSeparatedList
instance CssShow CommaSeparatedList where
  toCssText (CommaSeparatedList l) = toCssText l

newtype UnicodeRange = UnicodeRange Text deriving (Show, Eq, Ord, Generic)
instance CssShow UnicodeRange where
  toCssText (UnicodeRange t) = "U+" <> fromStrict t
instance ShowSpaceBetween UnicodeRange UnicodeRange where
  cssSpace _ _ = ", "

data FontFacePropEntry
  = UnicodeRangePropEntry (NonEmpty UnicodeRange)
  | FontFaceCommonEntry PropEntry
  deriving (Show, Eq, Ord, Generic)

instance CssShow FontFacePropEntry where
  toCssText = \case
    UnicodeRangePropEntry r ->
      "unicode-range: " <> toCssText r <> "; "
    FontFaceCommonEntry e ->
      toCssText e

instance ShowSpaceBetween FontFacePropEntry FontFacePropEntry where
  cssSpace _ _ = " "

data FontFace
  = FontFace
  { src :: CommaSeparatedList
  , optionalProps :: [FontFacePropEntry]
  }
  deriving (Show, Eq, Ord, Generic)

instance CssShow FontFace where
  toCssText ff =
    "@font-face { src: " <> toCssText ff.src <> "; " <> toCssText ff.optionalProps <> "}"

fromEitherM
  :: Applicative m
  => (e -> m a) -> Either e a -> m a
fromEitherM ef =
  \case
    Right v -> pure v
    Left e -> ef e

mkFontFace :: NonEmpty (Either SrcVal FontFacePropEntry) -> Either String FontFace
mkFontFace x =
  case partitionEithers $ toList x of
    ([sv], o) -> pure $ FontFace sv o
    ([], o) -> Left $ "There is a missing src descriptor of @font-face: " <> unpack (toCssText o)
    (msv, _) -> Left $ "Duplicated src descriptor of @font-face: " <> unpack (toCssText msv)
