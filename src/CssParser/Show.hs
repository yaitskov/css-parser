{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module CssParser.Show
  ( module X
  , CssShow (..)
  , ShowSpaceBetween (..)
  ) where

import CssParser.Prelude
import CssParser.TextMarshal as X

class ShowSpaceBetween (a :: Type) (b :: Type) where
  cssSpace :: forall aa -> forall bb -> (aa ~ a, bb ~ b) => LText

class CssShow a where
  toCssText :: a -> LText

instance (ShowSpaceBetween a a, CssShow a) => CssShow [a] where
  toCssText = intercalate (cssSpace a a) . fmap toCssText
{- HLINT ignore "Use concatMap" -}

instance (ShowSpaceBetween a a, CssShow a) => CssShow (NonEmpty a) where
  toCssText = toCssText . toList

instance CssShow () where
  toCssText () = " 0 ";

instance CssShow a => CssShow (Maybe a) where
  toCssText = maybe "" ((<> " ") . toCssText)

instance (CssShow a, CssShow b) => CssShow (Either a b) where
  toCssText = \case
    Left e -> toCssText e
    Right v -> toCssText v

instance (CssShow at, CssShow bt, ShowSpaceBetween at bt) => CssShow (These at bt) where
  toCssText = \case
    This a -> toCssText a
    That b -> toCssText b
    These a b -> toCssText a <> cssSpace at bt <> toCssText b
