module CssParser.At.Import where

import CssParser.Prelude
import CssParser.Show
import CssParser.Rule.Value


newtype Import = Import  { url :: Source } deriving (Show, Eq, Generic)

instance CssShow Import where
  toCssText (Import u) =
    "@import " <> toCssText u <> ";"
