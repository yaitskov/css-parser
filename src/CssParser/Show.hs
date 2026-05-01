{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module CssParser.Show
  ( module X
  , CssShow (..)
  , ShowSpaceBetween (..)
  , ShowParenthesis (..)
  , Embraced (..)
  , CslNe (..)
  , SslNe (..)
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

class ShowParenthesis (parent :: Type) (this :: Type) where
  left :: forall pp -> forall tt -> (pp ~ parent, tt ~ this) => LText
  right :: forall pp -> forall tt -> (pp ~ parent, tt ~ this) => LText


newtype CslNe a = CslNe (NonEmpty a) deriving (Show, Eq, Ord, Generic)

instance CssShow a => CssShow (CslNe a) where
  toCssText (CslNe l) =  intercalate ", " .  fmap toCssText $ toList l

newtype SslNe a = SslNe (NonEmpty a) deriving (Show, Eq, Ord, Generic)

instance CssShow a => CssShow (SslNe a) where
  toCssText (SslNe l) =  unwords .  fmap toCssText $ toList l

newtype Embraced a = Embraced a deriving (Show, Eq, Ord, Generic)
instance CssShow a => CssShow (Embraced a) where
  toCssText (Embraced a) = "("  <> toCssText a <> ")"

instance CssShow Integer where
  toCssText = numToText
