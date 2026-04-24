module CssParser.Ident where

import CssParser.Prelude
    ( Eq,
      Ord,
      Show,
      IsString,
      Generic,
      Semigroup((<>)),
      Text,
      fromStrict )
import CssParser.Show ( CssShow(..) )
import CssParser.Utils ( encodeIdentifier )

newtype Ident = Ident Text deriving newtype (Eq, Ord, Show, IsString)

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

newtype Var = Var Ident deriving newtype (Show, Eq, Ord) deriving (Generic)

instance CssShow Var where
  toCssText (Var i) = "--" <> toCssText i

data PropertyName
  = PropertyName Ident
  | VarProp Var
  deriving  (Eq, Ord, Show, Generic)

instance CssShow PropertyName where
  toCssText = \case
    PropertyName i -> toCssText i
    VarProp v -> toCssText v

-- instance CssShow PropertyName where
--   toCssText (PropertyName (Ident pn)) = fromStrict pn

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
