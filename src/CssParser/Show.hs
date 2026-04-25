module CssParser.Show
  ( module X
  , CssShow (..)
  ) where

import CssParser.Prelude
import CssParser.TextMarshal as X

class CssShow a where
  toCssText :: a -> LText

instance CssShow a => CssShow [a] where
  toCssText = concat . fmap toCssText
{- HLINT ignore "Use concatMap" -}

instance CssShow a => CssShow (NonEmpty a) where
  toCssText = concat . toList . fmap toCssText

instance CssShow () where
  toCssText () = " 0 ";

instance CssShow a => CssShow (Maybe a) where
  toCssText = maybe "" ((<> " ") . toCssText)

instance (CssShow a, CssShow b) => CssShow (Either a b) where
  toCssText = \case
    Left e -> toCssText e
    Right v -> toCssText v
