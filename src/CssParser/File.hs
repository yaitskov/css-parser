module CssParser.File where

import CssParser.At
import CssParser.Prelude
import CssParser.Rule
import CssParser.Show

data CssFile
  = CssFile
    { charset :: Maybe Charset
    , rules :: NonEmpty CssRule
    }
  deriving (Show, Eq, Generic)

instance CssShow CssFile where
  toCssText cf =
    maybe "" ((<> "\n") . toCssText) cf.charset <> intercalate "\n" (toCssText <$> toList cf.rules)
