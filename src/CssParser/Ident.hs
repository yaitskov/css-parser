module CssParser.Ident where

import CssParser.Prelude ( Eq, Ord, Show, IsString, Generic, Text )

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

newtype PropertyName = PropertyName Ident deriving newtype (Eq, Ord, Show, IsString) deriving (Generic)
