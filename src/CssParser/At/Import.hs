module CssParser.At.Import where

import CssParser.Prelude
import CssParser.Show

newtype Url = Url { unUrl :: Text }  deriving newtype (Show, Eq, Ord)
instance CssShow Url where
  toCssText (Url u) = "url(" <> encodeStringLiteral u <> ")"

data ImportSource
  = ImportSourceUrl Url
  | ImportSourceStr Text
  deriving (Show, Eq, Generic)

instance CssShow ImportSource where
  toCssText = \case
    ImportSourceUrl u -> toCssText u
    ImportSourceStr t -> encodeStringLiteral t

newtype Import
  = Import
  { url :: ImportSource
  } deriving (Show, Eq, Generic)

instance CssShow Import where
  toCssText (Import u) =
    "@import " <> toCssText u <> ";"
