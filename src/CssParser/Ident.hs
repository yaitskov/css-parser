module CssParser.Ident where

import CssParser.Prelude
import CssParser.Show
import CssParser.Utils ( encodeIdentifier )
import Data.Text (pack)

data BrowserPrefix
  = Moz
  | Na
  | Opera
  | WebKit
  | Apple
  | Microsoft
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance CssShow BrowserPrefix where
  toCssText = \case
    Moz -> "@-moz-"
    Na -> "@"
    Opera -> "@-o-"
    Apple -> "@-apple-"
    WebKit -> "@-webkit-"
    Microsoft -> "@-ms-"

newtype Ident = Ident Text deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

data TagName
  = TagName Ident
  | AmpersandTag
  | AsteriskTag
  | NoTag
  deriving (Eq, Ord, Show, Generic)

data Namespace
  = NoBar
  | NoNs
  | AsteriskNs
  | Namespace Ident
  deriving (Eq, Ord, Show, Generic)

data AttrName
  = AttrName
  { attrNs :: Namespace
  , attrName :: Ident
  } deriving (Eq, Ord, Show, Generic)

newtype Var = Var Ident deriving newtype (Show, Eq, Ord, IsString) deriving (Generic)

instance CssShow Var where
  toCssText (Var i) = "--" <> toCssText i

data PropertyName
  = PropertyName Ident
  | VarProp Var
  deriving  (Eq, Ord, Show, Generic)

instance IsString PropertyName where
  fromString ('-':'-':v) = VarProp (Var . Ident $ pack v)
  fromString o = PropertyName . Ident $ pack o

instance CssShow PropertyName where
  toCssText = \case
    PropertyName i -> toCssText i
    VarProp v -> toCssText v

instance CssShow Namespace where
  toCssText = \case
    NoBar -> ""
    NoNs  -> "|"
    AsteriskNs -> "*|"
    Namespace (Ident t) -> encodeIdentifier t <> "|"

instance CssShow TagName where
  toCssText = \case
    NoTag -> ""
    AsteriskTag -> "*"
    AmpersandTag -> "&"
    TagName (Ident lt) -> fromStrict lt

instance CssShow Ident where
  toCssText (Ident i) = encodeIdentifier i

newtype LayerName = LayerName Ident deriving newtype (Show, Eq, Ord, CssShow, IsString) deriving (Generic)

instance ShowSpaceBetween LayerName LayerName where
  cssSpace _ _ = ", "

newtype Charset = Charset Text deriving newtype (Show, Eq, Ord, IsString) deriving (Generic)

instance CssShow Charset where
  toCssText (Charset cs) = encodeStringLiteral cs
