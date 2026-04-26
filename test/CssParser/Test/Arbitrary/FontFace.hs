{-# OPTIONS_GHC -fconstraint-solver-iterations=24 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CssParser.Test.Arbitrary.FontFace where

import CssParser.Prelude
import CssParser.At.FontFace
import CssParser.Test.Arbitrary
import CssParser.Test.Arbitrary.Ident ()
import CssParser.Test.Arbitrary.Value ()
import CssParser.Test.Arbitrary.At ()

import Data.Text qualified as T

deriving via (GenericArbitrary CommaSeparatedList) instance Arbitrary CommaSeparatedList
deriving via (GenericArbitrary FontFace) instance Arbitrary FontFace
deriving via (GenericArbitrary FontFacePropEntry) instance Arbitrary FontFacePropEntry

unPatternLetter :: Gen Char
unPatternLetter = elements ( '?' : ['0' .. '9' ] <> ['a' .. 'f' ])

unPattern :: Gen Text
unPattern = do
  s <- unPatternLetter
  b <- sublistOf =<< vectorOf 3 unPatternLetter
  f <- maybeToList <$> elements [  Nothing, Just '0' ]
  p <- maybeToList <$> elements [  Nothing, Just '1' ]
  pure (pack $ p ++ f ++ b ++ [s])

unDoublePattern :: Gen Text
unDoublePattern = liftA2 (\a b -> a <> "-" <> b) unPattern unPattern

instance Arbitrary UnicodeRange where
  arbitrary = UnicodeRange <$> oneof [unPattern, unDoublePattern]
  shrink (UnicodeRange ur) =
    case T.dropEnd 1 ur of
      ur'->
        case T.unsnoc ur' of
          Just (ur'', '-') -> shrink (UnicodeRange ur'')
          Just (_, _) -> [UnicodeRange ur']
          Nothing -> []
